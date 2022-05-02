#include "Process.hh"

#include <functional>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>

#include "artifacts/Artifact.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "util/log.hh"

using std::function;
using std::list;
using std::make_shared;
using std::map;
using std::optional;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

Process::Process(Build& build,
                 const IRSource& source,
                 shared_ptr<Command> command,
                 pid_t pid,
                 Ref::ID cwd,
                 Ref::ID root,
                 map<int, FileDescriptor> fds,
                 optional<mode_t> umask) noexcept :
    _command(command), _pid(pid), _cwd(cwd), _root(root), _fds(fds) {
  // Set the process' default umask if one was not provided
  if (!umask.has_value()) {
    _umask = ::umask(0);
    ::umask(_umask);
  } else {
    _umask = umask.value();
  }

  // The new process has an open handle to each file descriptor in the _fds table
  for (auto& [index, desc] : _fds) {
    auto& [ref, cloexec] = desc;
    build.usingRef(source, _command, ref);
  }

  // The child process also duplicates references to the root and working directories
  // TODO: Do we need to track _exe here as well?
  build.usingRef(source, _command, _root);
  build.usingRef(source, _command, _cwd);
}

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

// Update a process' working directory
void Process::setWorkingDir(Build& build, const IRSource& source, Ref::ID ref) noexcept {
  // The process no longer saves its old cwd reference, and now saves the new working directory.
  // Encode this with close and open steps in the IR layer
  build.doneWithRef(source, _command, _cwd);
  build.usingRef(source, _command, ref);

  // Update the cwd
  _cwd = ref;
}

// Get a file descriptor entry
Ref::ID Process::getFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  ASSERT(iter != _fds.end()) << "Attempted to access an unknown fd " << fd << " in " << this;

  return std::get<0>(iter->second);
}

// Add a file descriptor entry
void Process::addFD(Build& build,
                    const IRSource& source,
                    int fd,
                    Ref::ID ref,
                    bool cloexec) noexcept {
  if (auto iter = _fds.find(fd); iter != _fds.end()) {
    WARN << "Overwriting an existing fd " << fd << " in " << this;
    auto& [old_ref, old_cloexec] = iter->second;
    WARN << "  Existing fd references " << getCommand()->getRef(old_ref)->getArtifact();
    build.doneWithRef(source, _command, old_ref);
    _fds.erase(iter);
  }

  // The command holds an additional handle to the provided Ref
  build.usingRef(source, _command, ref);

  // Add the entry to the process' file descriptor table
  _fds.emplace(fd, FileDescriptor(ref, cloexec));
}

// Close a file descriptor
void Process::closeFD(Build& build, const IRSource& source, int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    LOG(trace) << "Closing an unknown file descriptor " << fd << " in " << this;
  } else {
    auto& [old_ref, old_cloexec] = iter->second;
    build.doneWithRef(source, _command, old_ref);
    _fds.erase(iter);
  }
}

// Remove a file descriptor entry if it exists
bool Process::tryCloseFD(Build& build, const IRSource& source, int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter != _fds.end()) {
    auto& [old_ref, old_cloexec] = iter->second;
    build.doneWithRef(source, _command, old_ref);
    _fds.erase(iter);
    return true;
  }
  return false;
}

// Set a file descriptor's close-on-exec flag
void Process::setCloexec(int fd, bool cloexec) noexcept {
  auto iter = _fds.find(fd);
  ASSERT(iter != _fds.end())
      << "Attempted to set the cloexec flag for non-existent file descriptor " << fd;

  const auto& [ref, old_cloexec] = iter->second;
  iter->second = FileDescriptor{ref, cloexec};
}

// The process is creating a new child
shared_ptr<Process> Process::fork(Build& build, const IRSource& source, pid_t child_pid) noexcept {
  // Return the child process object
  return make_shared<Process>(build, source, _command, child_pid, _cwd, _root, _fds, _umask);
}

// The process is executing a new file
const shared_ptr<Command>& Process::exec(Build& build,
                                         const IRSource& source,
                                         Ref::ID exe_ref,
                                         vector<string> args) noexcept {
  // Build a map of the initial file descriptors for the child command.
  // key = file descriptor number
  // value = reference ID in the parent process
  map<int, Ref::ID> inherited_fds;

  // Loop over this process' file descriptors to find the ones that are inherited (not cloexec)
  for (const auto& [fd, desc] : _fds) {
    const auto& [ref, cloexec] = desc;

    // If this fd is inherited by the child, record it
    if (!cloexec) inherited_fds.emplace(fd, ref);
  }

  // Find (or create) a command for the child
  auto child = build.findCommand(_command, args, inherited_fds);

  // Build a mapping from parent refs to child refs to emit to the IR layer
  list<tuple<Ref::ID, Ref::ID>> refs;

  // The child inherits standard references
  refs.emplace_back(_root, Ref::Root);
  refs.emplace_back(_cwd, Ref::Cwd);
  refs.emplace_back(exe_ref, Ref::Exe);

  // The child also inherits references for initial file descriptors
  for (const auto& [fd, child_ref] : child->getInitialFDs()) {
    auto parent_ref = inherited_fds.at(fd);
    refs.emplace_back(parent_ref, child_ref);
  }

  // Inform the build of the launch
  build.launch(source, _command, child, refs);

  // The child is now launched in this process
  child->setLaunched(shared_from_this());

  // Now that the child has been launched, record that it is using all of its inherited refs
  // We only need to do this if the command is marked for run. If it is being emulated, it should
  // already have these IR steps
  if (child->mustRun()) {
    for (const auto& [parent_ref_id, child_ref_id] : refs) {
      build.usingRef(source, child, child_ref_id);
    }
  }

  // The parent command is no longer using any references in this process
  //_build.traceDoneWithRef(_command, exe_ref);
  build.doneWithRef(source, _command, _cwd);
  build.doneWithRef(source, _command, _root);

  for (const auto& [fd, desc] : _fds) {
    auto [ref, cloexec] = desc;
    build.doneWithRef(source, _command, ref);
  }

  // This process is now running the child
  _command = child;

  // This process is the primary process for its command
  _primary = true;

  // Clear the file descriptor map and fill it in with the child command's reference IDs
  _fds.clear();

  for (auto& [fd, ref] : child->getInitialFDs()) {
    _fds.emplace(fd, FileDescriptor{ref, false});
  }

  // Update the cwd and root references for the process to use refs from the new command
  _cwd = Ref::Cwd;
  _root = Ref::Root;

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
  // that participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.

  return _command;
}

// The process is exiting
void Process::exit(Build& build, const IRSource& source, int exit_status) noexcept {
  // We only need to handle the exit if the process hasn't already been marked as exited. That will
  // happen for skipped commands that are forced to exit.
  if (!_exited) {
    // Mark the process as exited
    _exited = true;

    // References to the cwd and root directories are closed
    build.doneWithRef(source, _command, _cwd);
    build.doneWithRef(source, _command, _root);

    // Any remaining file descriptors in this process are closed
    for (const auto& [index, desc] : _fds) {
      const auto& [ref, cloexec] = desc;
      build.doneWithRef(source, _command, ref);
    }

    // If this process was the primary for its command, trace the exit
    if (_primary) build.exit(source, _command, exit_status);
  }
}

// Set a callback that can be used to force this process to exit
void Process::waitForExit(function<void(int)> handler) noexcept {
  ASSERT(!_force_exit_callback) << "Process already has a callback to force exit";
  _force_exit_callback = handler;
}

// Force this process to exit instead of running an exec syscall
void Process::forceExit(int exit_status) noexcept {
  ASSERT(_force_exit_callback) << "Process does not have a callback to force exit";

  // Mark the process as exited
  _exited = true;

  // Invoke the force exit callback
  _force_exit_callback(exit_status);

  // Clear the callback in case it includes any references to allocated memory
  _force_exit_callback = function<void(int)>();
}