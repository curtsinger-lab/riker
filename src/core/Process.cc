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

Process::Process(pid_t pid, std::string cwd, std::shared_ptr<Command> command) :
    _pid(pid),
    _command(command),
    _cwd(cwd) {}

void Process::traceChdir(std::string newdir) { _cwd = newdir; }

void Process::traceChroot(std::string newroot) { _root = newroot; }

void Process::traceMmap(BuildGraph& graph, int fd) {
  auto& desc = _fds[fd];
  // Get the latest version of this file.
  // FIXME: This will do the wrong thing if a new file was placed at the same path after it was
  // opened by the current process.
  std::shared_ptr<File> f = desc.file->getLatestVersion();
  f->addMmap(shared_from_this());
  _mmaps.insert(f);

  if (desc.access_mode != O_WRONLY) getCommand()->addInput(f);
  if (desc.access_mode != O_RDONLY) getCommand()->addOutput(f);
}

void Process::traceClose(int fd) { _fds.erase(fd); }

std::shared_ptr<Process> Process::traceFork(pid_t child_pid) {
  auto child_proc = std::make_shared<Process>(child_pid, _cwd, _command);
  child_proc->_fds = _fds;
  return child_proc;
}

void Process::traceExec(BuildGraph& trace, std::string executable,
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
    file->removeMmap(shared_from_this());
  }

  // Mark the initial open file descriptors
  for (auto iter : _fds) {
    iter.second.file = trace.getLatestVersion(iter.second.location_index);
  }
  _command->setInitialFDs(_fds);

  // Assume that we can at any time write to stdout or stderr
  // TODO: Instead of checking whether we know about stdout and stderr,
  // tread the initial stdout and stderr properly as pipes
  if (_fds.find(fileno(stdout)) != _fds.end()) {
    trace.traceMmap(this->_pid, fileno(stdout));
  }
  if (_fds.find(fileno(stderr)) != _fds.end()) {
    trace.traceMmap(this->_pid, fileno(stderr));
  }
}

void Process::traceOpen(int fd, std::shared_ptr<File> f, int flags, mode_t mode) {
  int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);

  // Dropped this when moving out of ptrace.cc. This seems like it was wrong anyway...
  /*if ((flags & O_EXCL) != 0 || (flags & O_NOFOLLOW) != 0) {
    main_file.follow_links = false;
  }*/

  bool rewrite = ((flags & O_EXCL) != 0 || (flags & O_TRUNC) != 0);
  bool cloexec = (flags & O_CLOEXEC) != 0;

  if (rewrite && (f->getCreator() != getCommand() || f->isWritten())) {
    std::shared_ptr<File> newfile = f->createVersion();
    newfile->setCreator(getCommand());
    newfile->setWriter(nullptr);
    newfile->setMode(mode);
    _fds[fd] = FileDescriptor(f->getLocation(), newfile, access_mode, cloexec);
  } else {
    _fds[fd] = FileDescriptor(f->getLocation(), f, access_mode, cloexec);
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

void Process::traceSetCloexec(int fd, bool cloexec) {
  auto file = _fds.find(fd);
  if (file != _fds.end()) {
    file->second.cloexec = cloexec;
  }
}

void Process::traceExit() {
  for (auto f : _mmaps) {
    f->removeMmap(shared_from_this());
  }
}
