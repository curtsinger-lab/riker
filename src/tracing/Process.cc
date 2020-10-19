#include "Process.hh"

#include <memory>

#include "artifacts/Artifact.hh"
#include "data/FileDescriptor.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/RefResult.hh"
#include "util/log.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

// Update a process' working directory
void Process::setWorkingDir(shared_ptr<RefResult> ref) noexcept {
  ASSERT(ref->getArtifact()->getPath(false).has_value())
      << "Cannot set working directory to an artifact without a committed path";
  _cwd = ref;
}

// Get a file descriptor entry
FileDescriptor& Process::getFD(int fd) noexcept {
  ASSERT(_fds.find(fd) != _fds.end())
      << "Attempted to access an unknown fd " << fd << " in " << this;
  return _fds.at(fd);
}

// Add a file descriptor entry
FileDescriptor& Process::addFD(int fd,
                               shared_ptr<RefResult> ref,
                               AccessFlags flags,
                               bool cloexec) noexcept {
  if (auto iter = _fds.find(fd); iter != _fds.end()) {
    WARN << "Overwriting an existing fd " << fd << " in " << this;
    _build.traceClose(_command, iter->second.getRef());
    _fds.erase(iter);
  }

  // The command holds an additional handle to the provided RefResult
  ref->openedBy(_command);

  // Add the entry to the process' file descriptor table
  auto [iter, added] = _fds.emplace(fd, FileDescriptor(ref, flags, cloexec));
  return iter->second;
}

// Close a file descriptor
void Process::closeFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    LOG(trace) << "Closing an unknown file descriptor " << fd << " in " << this;
  } else {
    _build.traceClose(_command, iter->second.getRef());
    _fds.erase(iter);
  }
}

// Remove a file descriptor entry if it exists
void Process::tryCloseFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter != _fds.end()) {
    _build.traceClose(_command, iter->second.getRef());
    _fds.erase(iter);
  }
}

// The process is creating a new child
shared_ptr<Process> Process::fork(pid_t child_pid) noexcept {
  // The child process has a duplicate of every ref in the parent file descriptor table
  // Report these "open"s to the ref results
  for (auto& [index, desc] : _fds) {
    desc.getRef()->openedBy(_command);
  }

  // Return the child process object
  return make_shared<Process>(_build, _tracer, _command, child_pid, _cwd, _root, _fds);
}

// The process is executing a new file
void Process::exec(shared_ptr<RefResult> exe_ref,
                   vector<string> args,
                   vector<string> env) noexcept {
  // Build a map of the initial file descriptors for the child command
  // As we build this map, keep track of which file descriptors have to be erased from the
  // process' current map of file descriptors.
  map<int, FileDescriptor> initial_fds;
  list<int> to_erase;

  for (const auto& [index, fd] : _fds) {
    if (fd.isCloexec()) {
      // Report the close to the build
      _build.traceClose(_command, fd.getRef());

      // Remember this index so we can remove it later
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, FileDescriptor(fd.getRef(), fd.getFlags()));
    }
  }

  // Erase close-on-exec file descriptors from the FD map
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // Create the child command
  auto child = make_shared<Command>(exe_ref, args, initial_fds, _cwd, _root);

  // Loop over the initial FDs. These handles are shifting from the parent to child child.
  // We implement this by "opening" the handle in the child and "closing" it in the parent
  for (auto& [index, desc] : child->getInitialFDs()) {
    desc.getRef()->openedBy(child);
    desc.getRef()->closedBy(_command);
  }

  // Inform the build of the launch action
  _build.traceLaunch(_command, child);

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

  // Any remaining file descriptors in this process are closed
  for (auto& [index, desc] : _fds) {
    _build.traceClose(_command, desc.getRef());
  }
}