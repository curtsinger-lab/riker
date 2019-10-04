#include "Tracer.hh"

#include <cerrno>
#include <cstdio>
#include <utility>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"
#include "tracing/ptrace.hh"
#include "ui/log.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;

void Tracer::run(shared_ptr<Command> cmd) {
  pid_t pid = start_command(cmd, {});

  // TODO: Fix cwd handling
  _processes[pid] = make_shared<Process>(pid, ".", cmd, cmd->getInitialFDs());

  while (true) {
    int wait_status;
    pid_t child = wait(&wait_status);
    if (child == -1) {
      if (errno == ECHILD) {
        // ECHILD is returned when there are no children to wait on, which
        // is by far the simplest and most reliable signal we have for when
        // to exit (cleanly).
        break;
      } else {
        FAIL << "Error while waiting: " << ERR;
      }
    }

    trace_step(*this, child, wait_status);
  }
}

void Tracer::traceChdir(pid_t pid, string path) {
  _processes[pid]->_cwd = path;
}

void Tracer::traceChroot(pid_t pid, string path) {
  _processes[pid]->_root = path;
}

void Tracer::traceFork(pid_t pid, pid_t child_pid) {
  auto proc = _processes[pid];
  auto child_proc = make_shared<Process>(child_pid, proc->_cwd, proc->_command, proc->_fds);
  _processes[child_pid] = child_proc;
}

void Tracer::traceClone(pid_t pid, pid_t thread_id) {
  // Threads in the same process just appear as pid references to the same process
  _processes[thread_id] = _processes[pid];
}

void Tracer::traceExec(pid_t pid, string executable, const list<string>& args) {
  auto proc = _processes[pid];

  // Close all cloexec file descriptors
  for (auto fd_entry = proc->_fds.begin(); fd_entry != proc->_fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = proc->_fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }

  // Create the new command
  proc->_command = proc->_command->createChild(executable, args, proc->_fds);

  // The new command depends on its executable file
  _graph.getFile(executable)->readBy(proc->_command);
}

void Tracer::traceExit(pid_t pid) {}

void Tracer::traceOpen(pid_t pid, int fd, string path, int flags, mode_t mode) {
  auto f = _graph.getFile(path);
  auto proc = _processes[pid];

  int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);
  bool cloexec = (flags & O_CLOEXEC) != 0;
  bool truncated = (flags & O_TRUNC) != 0;
  bool created = (flags & O_CREAT) != 0;

  // Record the file descriptor entry
  proc->_fds[fd] = FileDescriptor(f, access_mode, cloexec);

  // Check if the file exists
  struct stat info;
  bool file_exists = (stat(path.c_str(), &info) == 0);

  // Log interaction differently depending on how the open call happened.
  if (file_exists && truncated) {
    // Truncate existing file
    f->truncatedBy(proc->_command);

  } else if (!file_exists && created) {
    // Create a new file
    f->createdBy(proc->_command);
  }

  // Otherwise, no interaction yet until we do something with the file
  
  LOG << proc->_command << " opened " << f;
}

void Tracer::traceClose(pid_t pid, int fd) {
  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;
  
  // Log the event if there was actually a valid file descriptor
  if (f) LOG << proc->_command << " closed " << proc->_fds[fd].file;
  
  proc->_fds.erase(fd);
}

void Tracer::tracePipe(pid_t pid, int fds[2], bool cloexec) {
  auto proc = _processes[pid];
  auto f = _graph.getPipe();
  
  f->createdBy(proc->_command);

  proc->_fds[fds[0]] = FileDescriptor(f, O_RDONLY, cloexec);
  proc->_fds[fds[1]] = FileDescriptor(f, O_WRONLY, cloexec);
}

void Tracer::traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
  auto proc = _processes[pid];
  
  auto target_fd = proc->_fds.find(new_fd);
  if (target_fd != proc->_fds.end()) {
    LOG << proc->_command << " closed " << proc->_fds[new_fd].file << " via dup";
  }
  
  auto duped_file = proc->_fds.find(duped_fd);
  if (duped_file == proc->_fds.end()) {
    proc->_fds.erase(new_fd);
  } else {
    proc->_fds[new_fd] = proc->_fds[duped_fd];
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
  // TODO
}

shared_ptr<File> Tracer::resolveFileRef(shared_ptr<Process> proc, struct file_reference& file) {
  shared_ptr<File> f;
  if (file.fd == AT_FDCWD) {
    f = _graph.getFile(file.path);
  } else {
    f = proc->_fds[file.fd].file;
  }

  if (!f) {
    WARN << "Unable to resolve file in " << proc->_command;
    WARN << "  fd: " << file.fd;
    WARN << "  path: " << file.path;
    WARN << "FDs:";
    for (auto entry : proc->_fds) {
      int index = entry.first;
      auto file = entry.second.file;
      
      if (file) {
        WARN << "  " << index << ": " << file;
      } else {
        WARN << "  " << index << ": INVALID";
      }
    }
  }

  return f;
}

void Tracer::traceRead(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->readBy(proc->_command);
}

void Tracer::traceModify(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->writtenBy(proc->_command);
}

void Tracer::traceTruncate(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->truncatedBy(proc->_command);
}

void Tracer::traceCreate(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->createdBy(proc->_command);
}

void Tracer::traceRemove(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->deletedBy(proc->_command);
}
