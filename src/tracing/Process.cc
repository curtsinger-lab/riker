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
    _fds.erase(iter);
  }

  auto [iter, added] = _fds.emplace(fd, FileDescriptor(ref, flags, cloexec));
  return iter->second;
}

// Close a file descriptor
void Process::closeFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    LOG(trace) << "Closing an unknown file descriptor " << fd << " in " << this;
  } else {
    _fds.erase(iter);
  }
}

// Remove a file descriptor entry if it exists
void Process::tryCloseFD(int fd) noexcept {
  _fds.erase(fd);
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
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, FileDescriptor(fd.getRef(), fd.getFlags()));
    }
  }
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // Create the child command
  auto child = make_shared<Command>(exe_ref, args, initial_fds, _cwd, _root);

  // Inform the build of the launch action
  _build.traceLaunch(_command, child);

  // This process is now running the child
  _command = child;

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
  // that participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}
