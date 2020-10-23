#include "Process.hh"

#include <memory>

#include "artifacts/Artifact.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "util/log.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

Process::Process(Build& build,
                 shared_ptr<Command> command,
                 pid_t pid,
                 shared_ptr<Ref> cwd,
                 shared_ptr<Ref> root,
                 map<int, FileDescriptor> fds) noexcept :
    _build(build), _command(command), _pid(pid), _cwd(cwd), _root(root), _fds(fds) {
  // The new process has an open handle to each file descriptor in the _fds table
  for (auto& [index, desc] : _fds) {
    auto& [ref, cloexec] = desc;
    _build.traceUsingRef(_command, ref);
  }

  // The child process also duplicates references to the root and working directories
  // TODO: Do we need to track _exe here as well?
  _build.traceUsingRef(_command, _root);
  _build.traceUsingRef(_command, _cwd);
}

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

// Update a process' working directory
void Process::setWorkingDir(shared_ptr<Ref> ref) noexcept {
  // The process no longer saves its old cwd reference, and now saves the new working directory.
  // Encode this with close and open steps in the IR layer
  _build.traceDoneWithRef(_command, _cwd);
  _build.traceUsingRef(_command, ref);

  // Update the cwd
  _cwd = ref;
}

// Get a file descriptor entry
const shared_ptr<Ref>& Process::getFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  ASSERT(iter != _fds.end()) << "Attempted to access an unknown fd " << fd << " in " << this;

  return std::get<0>(iter->second);
}

// Add a file descriptor entry
void Process::addFD(int fd, shared_ptr<Ref> ref, bool cloexec) noexcept {
  if (auto iter = _fds.find(fd); iter != _fds.end()) {
    WARN << "Overwriting an existing fd " << fd << " in " << this;
    auto& [old_ref, old_cloexec] = iter->second;
    _build.traceDoneWithRef(_command, old_ref);
    _fds.erase(iter);
  }

  // The command holds an additional handle to the provided Ref
  _build.traceUsingRef(_command, ref);

  // Add the entry to the process' file descriptor table
  _fds.emplace(fd, FileDescriptor(ref, cloexec));
}

// Close a file descriptor
void Process::closeFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    LOG(trace) << "Closing an unknown file descriptor " << fd << " in " << this;
  } else {
    auto& [old_ref, old_cloexec] = iter->second;
    _build.traceDoneWithRef(_command, old_ref);
    _fds.erase(iter);
  }
}

// Remove a file descriptor entry if it exists
void Process::tryCloseFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter != _fds.end()) {
    auto& [old_ref, old_cloexec] = iter->second;
    _build.traceDoneWithRef(_command, old_ref);
    _fds.erase(iter);
  }
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
shared_ptr<Process> Process::fork(pid_t child_pid) noexcept {
  // Return the child process object
  return make_shared<Process>(_build, _command, child_pid, _cwd, _root, _fds);
}

// The process is executing a new file
void Process::exec(shared_ptr<Ref> exe_ref, vector<string> args, vector<string> env) noexcept {
  // Build a map of the initial file descriptors for the child command
  // As we build this map, keep track of which file descriptors have to be erased from the
  // process' current map of file descriptors.
  map<int, shared_ptr<Ref>> initial_fds;
  list<int> to_erase;

  for (const auto& [index, desc] : _fds) {
    const auto& [ref, cloexec] = desc;
    if (cloexec) {
      // Report the close to the build
      _build.traceDoneWithRef(_command, ref);

      // Remember this index so we can remove it later
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, ref);
    }
  }

  // Erase close-on-exec file descriptors from the FD map
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // Inform the build of the launch action
  auto child = _build.traceLaunch(_command, exe_ref, args, initial_fds, _cwd, _root);

  // Loop over the initial FDs. These handles are shifting from the parent to the child.
  // We implement this by "opening" the handle in the child and "closing" it in the parent
  for (auto& [index, ref] : child->getInitialFDs()) {
    _build.traceUsingRef(child, ref);
    _build.traceDoneWithRef(_command, ref);
  }

  // The child gains references to root and cwd, which the parent then closes
  _build.traceUsingRef(child, _cwd);
  _build.traceDoneWithRef(_command, _cwd);

  _build.traceUsingRef(child, _root);
  _build.traceDoneWithRef(_command, _root);

  // TODO: Do we need to include a reference to the executable here?

  // This process is now running the child
  _command = child;

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
  // that participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}

// The process is exiting
void Process::exit() noexcept {
  // Mark the process as exited
  _exited = true;

  // References to the cwd and root directories are closed
  _build.traceDoneWithRef(_command, _cwd);
  _build.traceDoneWithRef(_command, _root);

  // Any remaining file descriptors in this process are closed
  for (const auto& [index, desc] : _fds) {
    const auto& [ref, cloexec] = desc;
    _build.traceDoneWithRef(_command, ref);
  }
}