#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <memory>
#include <string>
#include <vector>

#include <fcntl.h>
#include <limits.h>
#include <linux/audit.h>
#include <linux/bpf_common.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <syscall.h>
#include <unistd.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"
#include "tracing/syscalls.hh"
#include "ui/log.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;

#define ARRAY_COUNT(array) (sizeof(array) / sizeof(array[0]))

static string get_executable(pid_t pid);
static string read_tracee_string(pid_t process, uintptr_t tracee_pointer);
static uintptr_t read_tracee_data(pid_t process, uintptr_t tracee_pointer);
static pid_t launch_traced(char const* exec_path, char* const argv[],
                           vector<InitialFdEntry> initial_fds);

void Tracer::run(Command* cmd) {
  string exec_path = cmd->getExecutable();
  vector<char*> exec_argv;

  // Special handling for the root command
  if (cmd->isRoot()) {
    // By default, the root command is executed directly. However, it may not be executable.
    // In that case, start the root command using /bin/sh by default.

    // Check to see if the Dodofile is executable
    if (faccessat(AT_FDCWD, exec_path.c_str(), X_OK, AT_EACCESS)) {
      // Execute would fail. Can we read it and run with sh?
      if (faccessat(AT_FDCWD, exec_path.c_str(), R_OK, AT_EACCESS)) {
        // No. Print an error.
        FAIL << "Unable to access " << exec_path << ".\n"
             << "  This file must be executable, or a readable file that can be run by /bin/sh.";
      }

      // Root command file is readable but not executable. Run with /bin/sh
      exec_argv.push_back((char*)exec_path.c_str());
      exec_path = "/bin/sh";
    }
  }

  for (auto& arg : cmd->getArguments()) {
    exec_argv.push_back((char*)arg.c_str());
  }
  exec_argv.push_back(nullptr);
  pid_t pid = launch_traced(exec_path.c_str(), exec_argv.data(), {});

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

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        // Stopped on entry to a syscall
        handleSyscall(child);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8))) {
        // Stopped on entry to a fork call
        _fork(child);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // Stopped on entry to an exec call
        struct user_regs_struct registers;
        FAIL_IF(ptrace(PTRACE_GETREGS, child, nullptr, &registers))
            << "Failed to get registers: " << ERR;

        list<string> args;

        int child_argc = read_tracee_data(child, registers.rsp);
        for (int i = 0; i < child_argc; i++) {
          uintptr_t arg_ptr = read_tracee_data(child, registers.rsp + (1 + i) * sizeof(long));
          args.push_back(read_tracee_string(child, arg_ptr));
        }

        _execve(child, get_executable(child), args);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        // Stopped on entry to a clone call
        struct user_regs_struct registers;
        FAIL_IF(ptrace(PTRACE_GETREGS, child, nullptr, &registers))
            << "Failed to get registers: " << ERR;

        _clone(child, registers.SYSCALL_ARG1);

      } else {
        WARN << "Unhandled stop in process " << child;
        // We don't bother handling errors here, because any failure
        // just means that the child is somehow broken, and we shouldn't
        // continue processing it anyway.
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }

    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
      // Stopped on exit
      _exit(child);
    }
  }
}

void Tracer::resume(pid_t pid) {
  FAIL_IF(ptrace(PTRACE_CONT, pid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

long Tracer::finishSyscall(pid_t pid) {
  FAIL_IF(ptrace(PTRACE_SYSCALL, pid, nullptr, 0)) << "Failed to finish syscall: " << ERR;
  FAIL_IF(waitpid(pid, nullptr, 0) != pid) << "Unexpected child process stop";

  // Clear errno so we can check for errors
  errno = 0;
  long result = ptrace(PTRACE_PEEKUSER, pid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  return result;
}

unsigned long Tracer::getEventMessage(pid_t pid) {
  // Get the id of the new process
  unsigned long message;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, pid, nullptr, &message))
      << "Unable to read ptrace event message: " << ERR;
  return message;
}

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

// Some system calls are handled as aliases for these. See inline definitions in Tracer.hh.

void Tracer::_read(pid_t pid, int fd) {
  // Get the process and file
  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;
  
  // If there's no matching file descriptor, just resume and return
  if (!f) {
    resume(pid);
    return;
  }
  
  // Finish the syscall to find out if it succeeded, then resume the process
  int rc = finishSyscall(pid);
  resume(pid);

  // If the syscall failed, do nothing
  if (rc == -1) return;

  // Log the read
  f->readBy(proc->_command);
}

void Tracer::_write(pid_t pid, int fd) {
  // Get the process and file
  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;

  // If there was no matching file, resume the process and bail out
  if (!f) {
    resume(pid);
    return;
  }

  // There may be a write from this command (we might save a copy or take a fingerprint)
  f->mayWrite(proc->_command);

  // Finish the syscall and resume the process
  int rc = finishSyscall(pid);
  resume(pid);

  // If the syscall succeeded, record a write
  if (rc != -1) f->writtenBy(proc->_command);
}

void Tracer::_close(pid_t pid, int fd) {
  // NOTE: We assume close calls always succeed. Erasing a non-existent file descriptor is harmless

  // Resume the process
  resume(pid);

  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;

  // Log the event if there was actually a valid file descriptor
  if (f) LOG << proc->_command << " closed " << proc->_fds[fd].file;

  proc->_fds.erase(fd);
}

void Tracer::_mmap(pid_t pid, void* addr, size_t len, int prot, int flags, int fd, off_t off) {
  resume(pid);
  // TODO
}

int Tracer::_dup(pid_t pid, int fd) {
  // Finish the syscall to get the new file descriptor, then resume the process
  int newfd = finishSyscall(pid);
  resume(pid);

  // If the syscall failed, do nothing
  if (newfd == -1) return newfd;

  // Add the new entry for the duped fd
  auto proc = _processes[pid];
  proc->_fds[newfd] = proc->_fds[fd];

  // Return the new fd. This is helpful for handling some of the fcntl variants
  return newfd;
}

void Tracer::_dup2(pid_t pid, int oldfd, int newfd) {
  // Finish the syscall to get the return value, then resume
  int rc = finishSyscall(pid);
  resume(pid);

  // If the syscall failed, do nothing
  if (rc == -1) return;

  // Add the entry for the duped fd
  auto proc = _processes[pid];
  proc->_fds[newfd] = proc->_fds[oldfd];
}

void Tracer::_sendfile(pid_t pid, int out_fd, int in_fd) {
  // As with _write above, we may have to fingerprint the output file, although we won't know until
  // after the syscall (it could fail).
  auto proc = _processes[pid];
  auto in_f = proc->_fds[in_fd].file;
  auto out_f = proc->_fds[out_fd].file;

  // Take a fingerprint if we need one
  out_f->mayWrite(proc->_command);

  // Finish the system call and resume
  int rc = finishSyscall(pid);
  resume(pid);

  // If the syscall failed, do nothing
  if (rc == -1) return;

  in_f->readBy(proc->_command);
  out_f->writtenBy(proc->_command);
}

void Tracer::_clone(pid_t pid, int flags) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work
  
  // Get the new thread id and then resume execution
  pid_t new_pid = getEventMessage(pid);
  resume(pid);

  // TODO: Handle flags

  // Threads in the same process just appear as pid references to the same process
  _processes[new_pid] = _processes[pid];
}

void Tracer::_fork(pid_t pid) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work
  
  // Get the new process id and resume execution
  pid_t new_pid = getEventMessage(pid);
  resume(pid);

  // If the call failed, do nothing
  if (new_pid == -1) return;

  // Create a new process running the same command
  auto proc = _processes[pid];
  auto new_proc = make_shared<Process>(new_pid, proc->_cwd, proc->_command, proc->_fds);
  _processes[new_pid] = new_proc;
}

void Tracer::_execve(pid_t pid, string filename, const list<string>& args) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work
  
  // Resume the child
  resume(pid);

  // Get the process that issued the exec call
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
  proc->_command = proc->_command->createChild(filename, args, proc->_fds);

  // The new command depends on its executable file
  _graph.getFile(filename)->readBy(proc->_command);
}

void Tracer::_exit(pid_t pid) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work
  
  // Remove the process. No need to resume, since the process has exited
  _processes.erase(pid);
}

void Tracer::_fcntl(pid_t pid, int fd, int cmd, unsigned long arg) {
  if (cmd == F_DUPFD) {
    // Handle fcntl(F_DUPFD) as a dup call. The return value is the new fd.
    _dup(pid, fd);  // _dup will resume the process

  } else if (cmd == F_DUPFD_CLOEXEC) {
    // fcntl(F_DUPFD_CLOEXEC) is just like a dup call, followed by setting cloexec to true
    int newfd = _dup(pid, fd);  // _dup will resume the process
    auto proc = _processes[pid];
    proc->_fds[newfd].cloexec = true;

  } else if (cmd == F_SETFD) {
    resume(pid);
    // Set the cloexec flag using the argument flags
    auto proc = _processes[pid];
    proc->_fds[fd].cloexec = (arg & FD_CLOEXEC) != 0;

  } else {
    // Unhandled operation
    // TODO: Filter these stops out with BPF/seccomp
    resume(pid);
  }
}

void Tracer::_truncate(pid_t pid, string path, long length) {
  // Get the process and file
  auto proc = _processes[pid];
  auto f = _graph.getFile(path);

  // Notify the file of an upcoming change
  if (length == 0) {
    f->mayTruncate(proc->_command);
  } else {
    f->mayWrite(proc->_command);
  }
  
  // Finish the system call and resume
  int rc = finishSyscall(pid);
  resume(pid);

  // If the syscall failed, do nothing
  if (rc == -1) return;
  
  // Record the write or truncate
  if (length == 0) {
    f->truncatedBy(proc->_command);
  } else {
    f->writtenBy(proc->_command);
  }
}

void Tracer::_ftruncate(pid_t pid, int fd, long length) {
  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;

  if (length == 0) {
    f->truncatedBy(proc->_command);
  } else {
    f->writtenBy(proc->_command);
  }

  // Resume after logging so we have a chance to fingerprint
  resume(pid);
}

void Tracer::_chdir(pid_t pid, string filename) {
  resume(pid);

  // Update the current working directory
  auto proc = _processes[pid];
  proc->_cwd = filename;
}

void Tracer::_fchdir(pid_t pid, int fd) {
  resume(pid);

  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;
  if (f) proc->_cwd = f->getPath();
}

void Tracer::_rmdir(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_creat(pid_t pid, string pathname, mode_t mode) {
  resume(pid);
  // TODO
}

void Tracer::_fchmod(pid_t pid, int fd, mode_t mode) {
  resume(pid);
  // TODO
}

void Tracer::_fchown(pid_t pid, int fd, uid_t user, gid_t group) {
  resume(pid);
  // TODO
}

void Tracer::_lchown(pid_t pid, string filename, uid_t user, gid_t group) {
  resume(pid);
  // TODO
}

void Tracer::_chroot(pid_t pid, string filename) {
  resume(pid);
  // TODO
}

void Tracer::_setxattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_lsetxattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_getxattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_lgetxattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_listxattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_llistxattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_removexattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_lremovexattr(pid_t pid, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_openat(pid_t pid, int dfd, string filename, int flags, mode_t mode) {
  WARN_IF(dfd != AT_FDCWD) << "openat uses non-cwd directory fd";

  // Stat the file so we can see if it was created by the open call
  bool file_existed = true;
  struct stat statbuf;
  if (flags & O_CREAT) file_existed = (stat(filename.c_str(), &statbuf) == 0);

  // Run the syscall and save the resulting fd
  int fd = finishSyscall(pid);

  // Let the process continue
  resume(pid);

  // If the syscall failed, bail out
  if (fd == -1) return;

  // Extract relevant flags
  int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);
  bool cloexec = (flags & O_CLOEXEC) != 0;

  // Get the process and file
  auto proc = _processes[pid];
  auto f = _graph.getFile(filename);

  // Add the file to the file descriptor table
  proc->_fds[fd] = FileDescriptor(f, access_mode, cloexec);

  // Log creation and truncation interactions
  if (!file_existed) {
    f->createdBy(proc->_command);
  } else if (flags & O_TRUNC) {
    f->truncatedBy(proc->_command);
  }

  // Read and write interactions are added later, when the process actually reads or writes

  LOG << proc->_command << " opened " << f;
}

void Tracer::_mkdirat(pid_t pid, int dfd, string pathname, mode_t mode) {
  resume(pid);
  // TODO
}

void Tracer::_mknodat(pid_t pid, int dfd, string filename, mode_t mode, unsigned dev) {
  resume(pid);
  // TODO
}

void Tracer::_fchownat(pid_t pid, int dfd, string filename, uid_t user, gid_t group, int flag) {
  resume(pid);
  // TODO
}

void Tracer::_unlinkat(pid_t pid, int dfd, string pathname, int flag) {
  resume(pid);
  // TODO
}

void Tracer::_symlinkat(pid_t pid, string oldname, int newdfd, string newname) {
  resume(pid);
  // TODO
}

void Tracer::_readlinkat(pid_t pid, int dfd, string pathname) {
  resume(pid);
  // TODO
}

void Tracer::_fchmodat(pid_t pid, int dfd, string filename, mode_t mode, int flags) {
  resume(pid);
  // TODO
}

void Tracer::_splice(pid_t pid, int fd_in, loff_t off_in, int fd_out, loff_t off_out) {
  resume(pid);
  // TODO
}

void Tracer::_tee(pid_t pid, int fdin, int fdout, size_t len) {
  resume(pid);
  // TODO
}

void Tracer::_dup3(pid_t pid, int oldfd, int newfd, int flags) {
  resume(pid);
  // TODO
}

void Tracer::_pipe2(pid_t pid, int* fds, int flags) {
  resume(pid);
  // TODO
}

void Tracer::_renameat2(pid_t pid, int old_dfd, string oldpath, int new_dfd, string newpath,
                        int flags) {
  resume(pid);
  // TODO
}

void Tracer::_copy_file_range(pid_t pid, int fd_in, int _, int fd_out) {
  resume(pid);
  // TODO
}

/////////////////

void Tracer::handleSyscall(pid_t pid) {
  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, pid, nullptr, &regs)) << "Failed to get registers: " << ERR;

  switch (regs.SYSCALL_NUMBER) {
    case __NR_read:
      _read(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_write:
      _write(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_open:
      _open(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_close:
      _close(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_mmap:
      _mmap(pid, (void*)regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3, regs.SYSCALL_ARG4,
            regs.SYSCALL_ARG5, regs.SYSCALL_ARG6);
      break;

    case __NR_pread64:
      _pread64(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_pwrite64:
      _pwrite64(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_readv:
      _readv(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_writev:
      _writev(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_pipe:
      _pipe(pid, (int*)regs.SYSCALL_ARG1);
      break;

    case __NR_dup:
      _dup(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_dup2:
      _dup2(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_sendfile:
      _sendfile(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_fcntl:
      _fcntl(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_truncate:
      _truncate(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_ftruncate:
      _ftruncate(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_getdents:
      _getdents(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_chdir:
      _chdir(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_fchdir:
      _fchdir(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_rename:
      _rename(pid, read_tracee_string(pid, regs.SYSCALL_ARG1),
              read_tracee_string(pid, regs.SYSCALL_ARG2));
      break;

    case __NR_mkdir:
      _mkdir(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_rmdir:
      _rmdir(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_creat:
      _creat(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_unlink:
      _unlink(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_symlink:
      _symlink(pid, read_tracee_string(pid, regs.SYSCALL_ARG1),
               read_tracee_string(pid, regs.SYSCALL_ARG2));
      break;

    case __NR_readlink:
      _readlink(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_chmod:
      _chmod(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_fchmod:
      _fchmod(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_chown:
      _chown(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_fchown:
      _fchown(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_lchown:
      _lchown(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2,
              regs.SYSCALL_ARG3);
      break;

    case __NR_mknod:
      _mknod(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_chroot:
      _chroot(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_setxattr:
      _setxattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_lsetxattr:
      _lsetxattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_fsetxattr:
      _fsetxattr(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_getxattr:
      _getxattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_lgetxattr:
      _lgetxattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_fgetxattr:
      _fgetxattr(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_listxattr:
      _listxattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_llistxattr:
      _llistxattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_flistxattr:
      _flistxattr(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_removexattr:
      _removexattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_lremovexattr:
      _lremovexattr(pid, read_tracee_string(pid, regs.SYSCALL_ARG1));
      break;

    case __NR_fremovexattr:
      _fremovexattr(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_getdents64:
      _getdents64(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_openat:
      _openat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
              regs.SYSCALL_ARG4);
      break;

    case __NR_mkdirat:
      _mkdirat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
               regs.SYSCALL_ARG3);
      break;

    case __NR_mknodat:
      _mknodat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
               regs.SYSCALL_ARG3, regs.SYSCALL_ARG4);
      break;

    case __NR_fchownat:
      _fchownat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
                regs.SYSCALL_ARG3, regs.SYSCALL_ARG4, regs.SYSCALL_ARG5);
      break;

    case __NR_unlinkat:
      _unlinkat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
                regs.SYSCALL_ARG3);
      break;

    case __NR_renameat:
      _renameat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
                regs.SYSCALL_ARG3, read_tracee_string(pid, regs.SYSCALL_ARG4));
      break;

    case __NR_symlinkat:
      _symlinkat(pid, read_tracee_string(pid, regs.SYSCALL_ARG1), regs.SYSCALL_ARG2,
                 read_tracee_string(pid, regs.SYSCALL_ARG3));
      break;

    case __NR_readlinkat:
      _readlinkat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2));
      break;

    case __NR_fchmodat:
      _fchmodat(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
                regs.SYSCALL_ARG3, regs.SYSCALL_ARG4);
      break;

    case __NR_splice:
      _splice(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3, regs.SYSCALL_ARG4);
      break;

    case __NR_tee:
      _tee(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_vmsplice:
      _vmsplice(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_dup3:
      _dup3(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_pipe2:
      _pipe2(pid, (int*)regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_preadv:
      _preadv(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_pwritev:
      _pwritev(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_renameat2:
      _renameat2(pid, regs.SYSCALL_ARG1, read_tracee_string(pid, regs.SYSCALL_ARG2),
                 regs.SYSCALL_ARG3, read_tracee_string(pid, regs.SYSCALL_ARG4), regs.SYSCALL_ARG5);
      break;

    case __NR_copy_file_range:
      _copy_file_range(pid, regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_preadv2:
      _preadv2(pid, regs.SYSCALL_ARG1);
      break;

    case __NR_pwritev2:
      _pwritev2(pid, regs.SYSCALL_ARG1);
      break;

    default:
      FAIL << "Unhandled system call " << regs.SYSCALL_NUMBER;
  }
}

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

static string get_executable(pid_t pid) {
  char path_buffer[24];  // 24 is long enough for any integer PID
  sprintf(path_buffer, "/proc/%d/exe", pid);

  unique_ptr<char[]> buffer(nullptr);
  ssize_t capacity = 0;
  ssize_t bytes_read = 0;

  do {
    capacity += PATH_MAX;
    buffer = unique_ptr<char[]>(new char[capacity]);
    bytes_read = readlink(path_buffer, buffer.get(), capacity);
  } while (bytes_read == capacity);

  return string(buffer.get(), bytes_read);
}

static string read_tracee_string(pid_t process, uintptr_t tracee_pointer) {
  string output;

  // Loop to fetch words at a time until we find a null byte
  while (true) {
    // To properly check for errors when doing PEEKDATA, we need to clear then check errno,
    // since PEEKDATA could validly return -1.
    errno = 0;
    long data = ptrace(PTRACE_PEEKDATA, process, tracee_pointer + output.size(), nullptr);
    FAIL_IF(errno != 0) << "Failed to read string from traced process: " << ERR;

    // Copy in the data
    for (size_t i = 0; i < sizeof(long); i++) {
      char c = ((char*)&data)[i];
      if (c == '\0')
        return output;
      else
        output.push_back(c);
    }
  }
}

static uintptr_t read_tracee_data(pid_t process, uintptr_t tracee_pointer) {
  // Clear errno so we can detect errors
  errno = 0;
  uintptr_t result = ptrace(PTRACE_PEEKDATA, process, tracee_pointer, nullptr);
  FAIL_IF(errno != 0) << "Failed to read data from traced process: " << ERR;
  return result;
}

// Launch a program via `sh -c`, fully set up with ptrace and seccomp
// to be traced by the current process. launch_traced will return the
// PID of the newly created process, which should be running (or at least
// ready to be waited on) upon return.
static pid_t launch_traced(char const* exec_path, char* const argv[],
                           vector<InitialFdEntry> initial_fds) {
  // In terms of overall structure, this is a bog standard fork/exec spawning function.
  // We always launch the program with /bin/sh, similarly to `system`, which should
  // automatically handle resolving the correct instance of a program and supporting
  // features like redirection.
  //
  // The bulk of the complexity here is setting up tracing. Instead of just attaching
  // ptrace and asking our caller to repeatedly use PTRACE_SYSCALL to step through
  // syscalls, which can be incredibly expensive, we aim to only trigger a stop on
  // a useful subset. To accomplish this, we instally a seccomp-bpf filter that returns
  // SECCOMP_RET_TRACE when we want a stop.
  pid_t child_pid = fork();
  FAIL_IF(child_pid == -1) << "Failed to fork: " << ERR;

  if (child_pid == 0) {
    // This is the child

    // Set up FDs as requested. We assume that all parent FDs are marked CLOEXEC if
    // necessary and that there are no ordering constraints on duping (e.g. if the
    // child fd for one entry matches the parent fd of another).
    for (size_t fd_index = 0; fd_index < initial_fds.size(); fd_index++) {
      int rc = dup2(initial_fds[fd_index].parent_fd, initial_fds[fd_index].child_fd);
      FAIL_IF(rc != initial_fds[fd_index].child_fd) << "Failed to initialize fds: " << ERR;
    }

    // Allow ourselves to be traced by our parent
    FAIL_IF(ptrace(PTRACE_TRACEME, 0, nullptr, nullptr) != 0) << "Failed to start tracing: " << ERR;

    // Lock down the process so that we are allowed to
    // use seccomp without special permissions
    FAIL_IF(prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) << "Failed to allow seccomp: " << ERR;

    // Set up the seccomp filter. Since we are handling so
    // many syscalls, we don't want to just use a long list
    // of chained comparisons. Therefore, we first search
    // the syscall number to decide which block of 32 we are in,
    // then look up the result in a bitset.
    uint32_t bitset[11] = {0};
    for (size_t i = 0; i < ARRAY_COUNT(syscalls); i++) {
      bitset[syscalls[i] >> 5] |= 1 << (syscalls[i] & 0x1F);
    }

    // The actual binary BPF code
    struct sock_filter bpf_filter[] = {
        // Ensure that we are using the x86_64 syscall interface.
        // TODO: support other architectures
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, arch)),
        BPF_JUMP(BPF_JMP + BPF_JEQ + BPF_K, AUDIT_ARCH_X86_64, 1, 0),  // Check that this is x86_64
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_KILL),
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, nr)),
        BPF_JUMP(BPF_JMP + BPF_JSET + BPF_K, __X32_SYSCALL_BIT, 0,
                 1),  // And we are actually running a 64-bit program
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_KILL),

        // We stash the low bits of the syscall number for when we use it to look up
        // in a 32-bit set
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, nr)),
        BPF_STMT(BPF_ALU + BPF_AND + BPF_K, 0x1F),
        BPF_STMT(BPF_MISC + BPF_TAX, 0),

        // Look for the correct 32-bit block.
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, nr)),

#define BITSET_BLOCK(index, total)                                                                 \
  /* For each block, test if we are in the block */                                                \
  BPF_JUMP(BPF_JMP + BPF_JGE + BPF_K, 32 * (index + 1), 2,                                         \
           0), /* then load the appropriate bitset into the register */                            \
      BPF_STMT(BPF_LD + BPF_W + BPF_IMM,                                                           \
               bitset[index]), /* then jump to the testing code, past the catch-all instruction */ \
      BPF_JUMP(BPF_JMP + BPF_JA, (total - 1 - index) * 3 + 1, 0, 0)

        BITSET_BLOCK(0, 11),
        BITSET_BLOCK(1, 11),
        BITSET_BLOCK(2, 11),
        BITSET_BLOCK(3, 11),
        BITSET_BLOCK(4, 11),
        BITSET_BLOCK(5, 11),
        BITSET_BLOCK(6, 11),
        BITSET_BLOCK(7, 11),
        BITSET_BLOCK(8, 11),
        BITSET_BLOCK(9, 11),
        BITSET_BLOCK(10, 11),
        // The catch-all instruction will say not to trace any super-high syscall number
        // that we don't recognize
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_ALLOW),

        // Shift our bitset and extract the low bit to test the correct bit
        BPF_STMT(BPF_ALU + BPF_RSH + BPF_X, 0),
        BPF_JUMP(BPF_JMP + BPF_JSET + BPF_K, 1, 1 /* trace */, 0 /* allow */),
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_ALLOW),
        // TODO: Do more filtering here: for example, we don't care about private,
        // anonymous memory mappings or likely about interactions with stdin/stdout/stderr.
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_TRACE),
    };

    struct sock_fprog bpf_program;
    bpf_program.filter = bpf_filter;
    bpf_program.len = (unsigned short)ARRAY_COUNT(bpf_filter);

    // Actually enable the filter
    FAIL_IF(prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &bpf_program) != 0)
        << "Error enabling seccomp: " << ERR;

    // We need to stop here to give our parent a consistent time to add all the
    // options and correctly configure ptrace. Otherwise it will get a very confusing
    // response upon exec:
    // - Because PTRACE_O_TRACEEXEC has not been set (or worse, is racing with the call
    //   to exec), the parent may receive a SIGTRAP-stop on the exec.
    // - Because our seccomp program traps execve, the it will attempt to send our parent
    //   a seccomp stop when the parent is not configured to receive one.
    // Therefore, to ensure reliable behavior, we wait here, let the parent configure ptrace,
    // then continue to exec, which will raise two stops that the parent is (now) expecting
    // to handle.
    raise(SIGSTOP);

    // TODO: explicitly handle the environment
    execv(exec_path, argv);

    // This is unreachable, unless execv fails
    FAIL << "Failed to start traced program: " << ERR;
  }

  // In the parent. Wait for the child to reach its exec so that we
  // have a consistent point to play in. Here we haven't yet set
  // PTRACE_O_TRACEEXEC, so we will receive the legacy behavior of
  // a SIGTRAP.
  int wstatus;
  waitpid(child_pid, &wstatus, 0);  // Should correspond to raise(SIGSTOP)
  FAIL_IF(!WIFSTOPPED(wstatus) || WSTOPSIG(wstatus) != SIGSTOP) << "Unexpected stop from child";

  // Set up options to handle everything reliably. We do this before continuing
  // so that the actual running program has everything properly configured.
  int options = 0;
  options |= PTRACE_O_TRACEFORK | PTRACE_O_TRACECLONE | PTRACE_O_TRACEVFORK;  // Follow forks
  options |= PTRACE_O_TRACEEXEC;     // Handle execs more reliably
  options |= PTRACE_O_TRACESYSGOOD;  // When stepping through syscalls, be clear
  options |= PTRACE_O_TRACESECCOMP;  // Actually receive the syscall stops we requested

  FAIL_IF(ptrace(PTRACE_SETOPTIONS, child_pid, nullptr, options))
      << "Failed to set ptrace options: " << ERR;

  // Let the child restart to reach its exec.
  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  // Handle a stop from inside the exec
  waitpid(child_pid, &wstatus, 0);
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8)))
      << "Unexpected stop from child. Expected EXEC";

  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  return child_pid;
}
