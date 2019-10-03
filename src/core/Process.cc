#include "core/Process.hh"

#include <cstdio>
#include <list>
#include <memory>
#include <string>
#include <utility>

#include <fcntl.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "tracing/Tracer.hh"

void Process::traceMmap(int fd) {
  auto& desc = _fds[fd];
  // Get the latest version of this file.
  // FIXME: This will do the wrong thing if a new file was placed at the same path after it was
  // opened by the current process.
  std::shared_ptr<File> f = desc.file->getLatestVersion();
  f->addMmap(_command);
  _mmaps.insert(f);

  if (desc.access_mode != O_WRONLY) _command->traceRead(f);
  if (desc.access_mode != O_RDONLY) _command->traceModify(f);
}

void Process::traceExec(Tracer& tracer, BuildGraph& graph, std::string executable,
                        const std::list<std::string>& args) {
  _command = _command->createChild(executable, args);

  // Close all cloexec file descriptors
  for (auto fd_entry = _fds.begin(); fd_entry != _fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = _fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }

  // Close all mmaps, since the address space is replaced
  for (auto file : _mmaps) {
    file->removeMmap(_command);
  }

  // Mark the initial open file descriptors
  for (auto iter : _fds) {
    iter.second.file = graph.getLatestVersion(iter.second.location_index);
  }
  _command->setInitialFDs(_fds);

  // Assume that we can at any time write to stdout or stderr
  // TODO: Instead of checking whether we know about stdout and stderr,
  // tread the initial stdout and stderr properly as pipes
  if (_fds.find(fileno(stdout)) != _fds.end()) {
    tracer.traceMmap(_pid, fileno(stdout));
  }
  if (_fds.find(fileno(stderr)) != _fds.end()) {
    tracer.traceMmap(_pid, fileno(stderr));
  }
}

void Process::tracePipe(int fd1, int fd2, std::shared_ptr<File> f, bool cloexec) {
  _fds[fd1] = FileDescriptor(f->getLocation(), f, O_RDONLY, cloexec);
  _fds[fd2] = FileDescriptor(f->getLocation(), f, O_WRONLY, cloexec);
}

void Process::traceDup(int fd, int new_fd, bool cloexec) {
  auto duped_file = _fds.find(fd);
  if (duped_file == _fds.end()) {
    _fds.erase(new_fd);
  } else {
    _fds[new_fd] = FileDescriptor(duped_file->second.location_index, duped_file->second.file,
                                  duped_file->second.access_mode, cloexec);
  }
}
