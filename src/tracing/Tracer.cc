#include "Tracer.hh"

#include <fcntl.h>
#include <sys/types.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"

void Tracer::newProcess(pid_t pid, std::shared_ptr<Command> cmd) {
  Process* proc = new Process(pid, ".", cmd, _graph.getDefaultFds());  // TODO: Fix cwd handling
  _processes.emplace(pid, proc);
}

void Tracer::traceChdir(pid_t pid, std::string path) {
  _processes[pid]->_cwd = path;
}

void Tracer::traceChroot(pid_t pid, std::string path) {
  _processes[pid]->_root = path;
}

void Tracer::traceFork(pid_t pid, pid_t child_pid) {
  auto proc = _processes[pid];
  auto child_proc = std::make_shared<Process>(child_pid, proc->_cwd, proc->_command, proc->_fds);
  _processes[child_pid] = child_proc;
}

void Tracer::traceClone(pid_t pid, pid_t thread_id) {
  // Threads in the same process just appear as pid references to the same process
  _processes[thread_id] = _processes[pid];
}

void Tracer::traceExec(pid_t pid, std::string executable, const std::list<std::string>& args) {
  auto proc = _processes[pid];
  
  // Close all cloexec file descriptors
  for (auto fd_entry = proc->_fds.begin(); fd_entry != proc->_fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = proc->_fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }
  
  // Close all mmaps, since the address space is replaced
  for (auto file : proc->_mmaps) {
    file->removeMmap(proc->_command);
  }
  
  // Create the new command
  proc->_command = proc->_command->createChild(executable, args);

  // Mark the initial open file descriptors
  for (auto iter : proc->_fds) {
    iter.second.file = _graph.getLatestVersion(iter.second.location_index);
  }
  proc->_command->setInitialFDs(proc->_fds);
  
  // Assume that we can at any time write to stdout or stderr
  // TODO: Instead of checking whether we know about stdout and stderr,
  // tread the initial stdout and stderr properly as pipes
  if (proc->_fds.find(fileno(stdout)) != proc->_fds.end()) {
    traceMmap(pid, fileno(stdout));
  }
  if (proc->_fds.find(fileno(stderr)) != proc->_fds.end()) {
    traceMmap(pid, fileno(stderr));
  }
  
  // The new command depends on its executable file
  auto f = _graph.getFile(executable);
  proc->_command->traceRead(f);
}

void Tracer::traceExit(pid_t pid) {
  auto proc = _processes[pid];
  for (auto f : proc->_mmaps) {
    f->removeMmap(proc->_command);
  }
}

void Tracer::traceOpen(pid_t pid, int fd, std::string path, int flags, mode_t mode) {
  auto f = _graph.getFile(path);
  auto proc = _processes[pid];

  int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);

  // Dropped this when moving out of ptrace.cc. This seems like it was wrong anyway...
  /*if ((flags & O_EXCL) != 0 || (flags & O_NOFOLLOW) != 0) {
    main_file.follow_links = false;
  }*/

  bool rewrite = ((flags & O_EXCL) != 0 || (flags & O_TRUNC) != 0);
  bool cloexec = (flags & O_CLOEXEC) != 0;

  if (rewrite && (f->getCreator() != proc->_command || f->isWritten())) {
    auto newfile = f->createVersion(proc->_command);
    newfile->setMode(mode);
    proc->_fds[fd] = FileDescriptor(f->getLocation(), newfile, access_mode, cloexec);
  } else {
    proc->_fds[fd] = FileDescriptor(f->getLocation(), f, access_mode, cloexec);
  }
}

void Tracer::traceClose(pid_t pid, int fd) {
  _processes[pid]->_fds.erase(fd);
}

void Tracer::tracePipe(pid_t pid, int fds[2], bool cloexec) {
  auto proc = _processes[pid];

  auto f = _graph.getPipe(proc->_command);

  proc->_fds[fds[0]] = FileDescriptor(f->getLocation(), f, O_RDONLY, cloexec);
  proc->_fds[fds[1]] = FileDescriptor(f->getLocation(), f, O_WRONLY, cloexec);
}

void Tracer::traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
  auto proc = _processes[pid];
  auto duped_file = proc->_fds.find(duped_fd);
  if (duped_file == proc->_fds.end()) {
    proc->_fds.erase(new_fd);
  } else {
    proc->_fds[new_fd] = FileDescriptor(duped_file->second.location_index, duped_file->second.file,
                                        duped_file->second.access_mode, cloexec);
  }
}

void Tracer::traceSetCloexec(pid_t pid, int fd, bool cloexec) {
  auto proc = _processes[pid];
  auto file = proc->_fds.find(fd);
  if (file != proc->_fds.end()) {
    file->second.cloexec = cloexec;
  }
}

void Tracer::traceMmap(pid_t pid, int fd) {
  auto proc = _processes[pid];
  
  auto& desc = proc->_fds[fd];
  // Get the latest version of this file.
  // FIXME: This will do the wrong thing if a new file was placed at the same path after it was
  // opened by the current process.
  auto f = desc.file->getLatestVersion();
  f->addMmap(proc->_command);
  proc->_mmaps.insert(f);

  if (desc.access_mode != O_WRONLY) proc->_command->traceRead(f);
  if (desc.access_mode != O_RDONLY) proc->_command->traceModify(f);
}

void Tracer::traceRead(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  shared_ptr<File> f;
  if (file.fd == AT_FDCWD)
    f = _graph.getFile(file.path);
  else
    f = proc->_fds[file.fd].file;
  proc->_command->traceRead(f);
}

void Tracer::traceModify(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  shared_ptr<File> f;
  if (file.fd == AT_FDCWD)
    f = _graph.getFile(file.path);
  else
    f = proc->_fds[file.fd].file;
  proc->_command->traceModify(f);
}

void Tracer::traceCreate(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  shared_ptr<File> f;
  if (file.fd == AT_FDCWD)
    f = _graph.getFile(file.path);
  else
    f = proc->_fds[file.fd].file;
  proc->_command->traceCreate(f);
}

void Tracer::traceRemove(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  shared_ptr<File> f;
  if (file.fd == AT_FDCWD)
    f = _graph.getFile(file.path);
  else
    f = proc->_fds[file.fd].file;
  proc->_command->traceRemove(f);
}
