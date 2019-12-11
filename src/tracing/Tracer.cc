#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <list>
#include <memory>
#include <string>
#include <utility>
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
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <syscall.h>
#include <unistd.h>

#include "core/Artifact.hh"
#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "tracing/syscalls.hh"
#include "ui/log.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;

static pid_t launch_traced(char const* exec_path, char* const argv[],
                           vector<InitialFdEntry> initial_fds);

void Tracer::run(shared_ptr<Command> cmd) {
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
  _processes[pid] = make_shared<Process>(_graph, pid, ".", cmd, cmd->getInitialFDs());

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

    auto p = _processes[child];

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        // Stopped on entry to a syscall
        handleSyscall(p);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8))) {
        // Stopped on entry to a fork call
        handleFork(p);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // Stopped on entry to an exec call
        auto regs = p->getRegisters();

        list<string> args;

        int child_argc = p->readData(regs.rsp);
        for (int i = 0; i < child_argc; i++) {
          uintptr_t arg_ptr = p->readData(regs.rsp + (1 + i) * sizeof(long));
          args.push_back(p->readString(arg_ptr));
        }

        p->_exec(p->getExecutable(), args);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        // Stopped on entry to a clone call
        auto regs = p->getRegisters();
        handleClone(p, regs.SYSCALL_ARG1);

      } else {
        WARN << "Unhandled stop in process " << child;
        // We don't bother handling errors here, because any failure
        // just means that the child is somehow broken, and we shouldn't
        // continue processing it anyway.
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }

    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
      // Stopped on exit
      handleExit(p);
    }
  }
}

void Tracer::Process::resume() {
  FAIL_IF(ptrace(PTRACE_CONT, _pid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

long Tracer::Process::finishSyscall() {
  FAIL_IF(ptrace(PTRACE_SYSCALL, _pid, nullptr, 0)) << "Failed to finish syscall: " << ERR;
  FAIL_IF(waitpid(_pid, nullptr, 0) != _pid) << "Unexpected child process stop";

  // Clear errno so we can check for errors
  errno = 0;
  long result = ptrace(PTRACE_PEEKUSER, _pid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  return result;
}

unsigned long Tracer::Process::getEventMessage() {
  // Get the id of the new process
  unsigned long message;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, _pid, nullptr, &message))
      << "Unable to read ptrace event message: " << ERR;
  return message;
}

path Tracer::Process::resolvePath(path p, int at, bool follow_links) {
  // TODO: Handle chroot-ed processes correctly
  // TODO: Handle links (followed or unfollowed)

  // Just normalize absolute paths
  if (p.is_absolute()) return p.lexically_normal();

  // Otherwise the path is relative somehow. We need to find out what it's relative to.
  // By default, use the current directory
  path base = _cwd;

  // But if the file is not relative to cwd, get the path for the specified base
  if (at != AT_FDCWD) base = _fds[at].artifact->getPath();

  return (base / p).lexically_normal();
}

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

// Some system calls are handled as aliases for these. See inline definitions in Tracer.hh.

void Tracer::Process::_read(int fd) {
  // Get the process and file
  auto f = _fds[fd].artifact;

  // If there's no matching file descriptor, just resume and return
  if (!f) {
    resume();
    return;
  }

  // We can't wait for the syscall to finish here because of this scenario:
  //  fd may be the read end of a pipe that is currently empty. The process that will write to the
  //  pipe is also blocked, but we're not handling it now. In that case, the syscall will not
  //  finish until we resume the *other* process. To handle this case correctly we'd need to place
  //  a wait for any child after resuming the blocked process. pre_ and post_ hooks for syscalls
  //  would work, but we don't always need them. Threads would also work, btu that creates other
  //  problems.
  resume();

  // Log the read
  f->readBy(_command);
}

void Tracer::Process::_write(int fd) {
  // Get the process and file
  auto f = _fds[fd].artifact;

  // If there was no matching file, resume the process and bail out
  if (!f) {
    resume();
    return;
  }

  // There may be a write from this command (we might save a copy or take a fingerprint)
  f->mayWrite(_command);

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  // If the syscall succeeded, record a write
  if (rc != -1) f->writtenBy(_command);
}

void Tracer::Process::_close(int fd) {
  // NOTE: We assume close calls always succeed. Erasing a non-existent file descriptor is harmless

  // Resume the process
  resume();

  auto f = _fds[fd].artifact;

  // Log the event if there was actually a valid file descriptor
  if (f) LOG << _command << " closed " << _fds[fd].artifact;

  _fds.erase(fd);
}

void Tracer::Process::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) {
  // Skip anonymous mappings. We never need to handle these because they only allow communication
  // within a single command.
  if (fd == -1) {
    resume();
    return;
  }

  void* rc = (void*)finishSyscall();
  resume();
  
  // If the map failed there's nothing to log
  if (rc == MAP_FAILED) return;
  
  auto descriptor = _fds[fd];
  auto f = descriptor.artifact;
  bool writable = prot & PROT_WRITE;
  // The mapping is only writable if the file was also open in writable mode
  writable &= (descriptor.access_mode & O_WRONLY) || (descriptor.access_mode & O_RDWR);

  // Record the mmap
  f->mappedBy(_command, writable);

  // Also track the mmap in the process so we can notify the file of an unmap later
  _mmaps.insert(f);
}

int Tracer::Process::_dup(int fd) {
  // Finish the syscall to get the new file descriptor, then resume the process
  int newfd = finishSyscall();
  resume();

  // If the syscall failed, do nothing
  if (newfd == -1) return newfd;

  // Add the new entry for the duped fd
  _fds[newfd] = _fds[fd];

  // Return the new fd. This is helpful for handling some of the fcntl variants
  return newfd;
}

void Tracer::Process::_sendfile(int out_fd, int in_fd) {
  // As with _write above, we may have to fingerprint the output file, although we won't know until
  // after the syscall (it could fail).
  auto in_f = _fds[in_fd].artifact;
  auto out_f = _fds[out_fd].artifact;

  // Take a fingerprint if we need one
  out_f->mayWrite(_command);

  // Finish the system call and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, do nothing
  if (rc == -1) return;

  in_f->readBy(_command);
  out_f->writtenBy(_command);
}

void Tracer::Process::_exec(string filename, const list<string>& args) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Resume the child
  resume();

  // Close all cloexec file descriptors
  for (auto fd_entry = _fds.begin(); fd_entry != _fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = _fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }

  // Create the new command
  _command = _command->createChild(filename, args, _fds);

  // The new command depends on its executable file
  _graph.getArtifact(resolvePath(filename))->readBy(_command);

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes that
  // participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}

void Tracer::Process::_fcntl(int fd, int cmd, unsigned long arg) {
  if (cmd == F_DUPFD) {
    // Handle fcntl(F_DUPFD) as a dup call. The return value is the new fd.
    _dup(fd);  // _dup will resume the process

  } else if (cmd == F_DUPFD_CLOEXEC) {
    // fcntl(F_DUPFD_CLOEXEC) is just like a dup call, followed by setting cloexec to true
    int newfd = _dup(fd);  // _dup will resume the process
    _fds[newfd].cloexec = true;

  } else if (cmd == F_SETFD) {
    resume();
    // Set the cloexec flag using the argument flags
    _fds[fd].cloexec = (arg & FD_CLOEXEC) != 0;

  } else {
    // Some other operation we do not need to handle
    // TODO: Filter these stops out with BPF/seccomp
    resume();
  }
}

void Tracer::Process::_truncate(string pathname, long length) {
  // Get the process and file
  auto f = _graph.getArtifact(resolvePath(pathname));

  // Notify the file of an upcoming change
  if (length == 0) {
    f->mayTruncate(_command);
  } else {
    f->mayWrite(_command);
  }

  // Finish the system call and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, do nothing
  if (rc == -1) return;

  // Record the write or truncate
  if (length == 0) {
    f->truncatedBy(_command);
  } else {
    f->writtenBy(_command);
  }
}

void Tracer::Process::_ftruncate(int fd, long length) {
  auto f = _fds[fd].artifact;

  if (length == 0) {
    f->truncatedBy(_command);
  } else {
    f->writtenBy(_command);
  }

  // Resume after logging so we have a chance to fingerprint
  resume();
}

void Tracer::Process::_chdir(string filename) {
  int rc = finishSyscall();
  resume();

  // Update the current working directory if the chdir call succeeded
  if (rc == 0) _cwd = resolvePath(filename);
}

void Tracer::Process::_fchdir(int fd) {
  int rc = finishSyscall();
  resume();

  if (rc == 0) {
    auto f = _fds[fd].artifact;
    WARN_IF(!f) << "Unable to locate file used in fchdir";
    _cwd = f->getPath();
  }
}

void Tracer::Process::_lchown(string filename, uid_t user, gid_t group) {
  // Resolve the path without following links, then get the file tracking object
  auto f = _graph.getArtifact(resolvePath(filename, AT_FDCWD, false));

  // Indicate that we may write this file
  f->mayWrite(_command);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, bail out
  if (rc == -1) return;

  // Record a write
  f->writtenBy(_command);
}

void Tracer::Process::_chroot(string filename) {
  string newroot = resolvePath(filename);
  auto f = _graph.getArtifact(newroot);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  if (rc != -1) {
    // Update the process root
    _root = newroot;

    // A directory must exist to
    f->readBy(_command);
  }
}

void Tracer::Process::_setxattr(string pathname) {
  // Get the process and file
  auto f = _graph.getArtifact(resolvePath(pathname));

  // Notify the file that it may be written
  f->mayWrite(_command);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  if (rc != -1) f->writtenBy(_command);
}

void Tracer::Process::_lsetxattr(string pathname) {
  // Get the process and file
  // Same as setxattr, except we do not follow links
  auto f = _graph.getArtifact(resolvePath(pathname, AT_FDCWD, false));

  // Notify the file that it may be written
  f->mayWrite(_command);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  if (rc != -1) f->writtenBy(_command);
}

void Tracer::Process::_getxattr(string pathname) {
  // Get the process and file
  auto f = _graph.getArtifact(resolvePath(pathname));

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  if (rc != -1) f->readBy(_command);
}

void Tracer::Process::_lgetxattr(string pathname) {
  // Get the process and file
  // Same as getxattr, except we don't follow links
  auto f = _graph.getArtifact(resolvePath(pathname, AT_FDCWD, false));

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  if (rc != -1) f->readBy(_command);
}

void Tracer::Process::_openat(int dfd, string filename, int flags, mode_t mode) {
  path p = resolvePath(filename, dfd);

  // Stat the file so we can see if it was created by the open call
  bool file_existed = true;
  struct stat statbuf;
  if (flags & O_CREAT) file_existed = (stat(p.c_str(), &statbuf) == 0);

  // Run the syscall and save the resulting fd
  int fd = finishSyscall();

  // Let the process continue
  resume();

  // If the syscall failed, bail out
  if (fd == -1) return;

  // Extract relevant flags
  int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);
  bool cloexec = (flags & O_CLOEXEC) != 0;

  // Get the process and file
  auto f = _graph.getArtifact(p);

  // Add the file to the file descriptor table
  _fds[fd] = FileDescriptor(f, access_mode, cloexec);

  // Log creation and truncation interactions
  if (!file_existed) {
    f->createdBy(_command);
  } else if (flags & O_TRUNC) {
    f->truncatedBy(_command);
  }

  // Read and write interactions are added later, when the process actually reads or writes

  LOG << _command << " opened " << f;
}

void Tracer::Process::_mkdirat(int dfd, string pathname, mode_t mode) {
  path p = resolvePath(pathname, dfd);

  // Stat the directory so we can see if it was created by the mkdirat call
  bool dir_existed = true;
  struct stat statbuf;
  dir_existed = (stat(p.c_str(), &statbuf) == 0);

  // Run the syscall
  int rc = finishSyscall();
  resume();

  // If the call failed, do nothing
  if (rc) return;

  if (!dir_existed) {
    auto f = _graph.getArtifact(p, Artifact::Type::DIRECTORY);
    f->createdBy(_command);

    auto parent_dir = _graph.getArtifact(p.parent_path(), Artifact::Type::DIRECTORY);
    parent_dir->readBy(_command);
  }

  // TODO: if creation failed, does this command now depend on the directory that already exists?
  // TODO: creating any file or directory should depend on its parent directory
}

void Tracer::Process::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) {
  // TODO: Handle or skip device nodes. FIFOs may need special handling?

  int rc = finishSyscall();
  resume();

  // Give up if the syscall fails
  if (rc != 0) return;

  string p = resolvePath(filename, dfd);
  auto f = _graph.getArtifact(p);

  f->createdBy(_command);
}

void Tracer::Process::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flag) {
  Artifact* f;

  // An empty path means just use dfd as the file
  if (flag & AT_EMPTY_PATH) {
    f = _fds[dfd].artifact;
  } else {
    // Are we following links or not?
    bool follow_links = (flag & AT_SYMLINK_NOFOLLOW) == 0;

    // Resolve the path, then get the file tracking object
    f = _graph.getArtifact(resolvePath(filename, dfd, follow_links));
  }

  // Indicate that we may write this file
  f->mayWrite(_command);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, bail out
  if (rc == -1) return;

  // Record a write
  f->writtenBy(_command);
}

void Tracer::Process::_unlinkat(int dfd, string pathname, int flag) {
  string p = resolvePath(pathname, dfd);
  auto f = _graph.getArtifact(p);

  f->mayDelete(_command);

  int rc = finishSyscall();
  resume();

  if (rc == 0) f->deletedBy(_command);
}

void Tracer::Process::_symlinkat(string oldname, int newdfd, string newname) {
  resume();
  // TODO
}

void Tracer::Process::_readlinkat(int dfd, string pathname) {
  resume();
  // TODO
}

void Tracer::Process::_fchmodat(int dfd, string filename, mode_t mode, int flags) {
  // Are we following links or not? Depends on the flags.
  bool follow_links = (flags & AT_SYMLINK_NOFOLLOW) == 0;

  // Find the file object
  auto f = _graph.getArtifact(resolvePath(filename, dfd, follow_links));

  // Indicate that we may write this file
  f->mayWrite(_command);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, bail out
  if (rc != 0) return;

  // Record the write
  f->writtenBy(_command);
}

void Tracer::Process::_tee(int fd_in, int fd_out) {
  auto input_f = _fds[fd_in].artifact;
  auto output_f = _fds[fd_out].artifact;

  // If either file doesn't exist, bail out
  if (!input_f || !output_f) {
    resume();
    return;
  }

  // Indicate that we may write the file
  output_f->mayWrite(_command);

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, bail
  if (rc == -1) return;

  // Record the read and write operations
  input_f->readBy(_command);
  output_f->writtenBy(_command);
}

void Tracer::Process::_dup3(int oldfd, int newfd, int flags) {
  // Finish the syscall to get the return value, then resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, do nothing
  if (rc == -1) return;

  // Add the entry for the duped fd
  _fds[newfd] = _fds[oldfd];

  // Set the cloexec flag if specified in the flags
  if (flags & O_CLOEXEC) _fds[newfd].cloexec = true;
}

void Tracer::Process::_pipe2(int* fds, int flags) {
  int rc = finishSyscall();

  // Bail out if the syscall failed
  if (rc) return;

  // Read the file descriptors
  int read_pipefd = readData((uintptr_t)fds);
  int write_pipefd = readData((uintptr_t)fds + sizeof(int));

  resume();

  // Create a pipe
  auto p = _graph.getPipe();

  // Will these pipe file descriptors be closed on exec?
  bool cloexec = (flags & O_CLOEXEC) != 0;

  // Create the FD records
  _fds[read_pipefd] = FileDescriptor(p, O_RDONLY, cloexec);
  _fds[write_pipefd] = FileDescriptor(p, O_WRONLY, cloexec);

  p->createdBy(_command);
}

void Tracer::Process::_renameat2(int old_dfd, string oldpath, int new_dfd, string newpath,
                                 int flags) {
  auto old_f = _graph.getArtifact(resolvePath(oldpath, old_dfd));
  string new_path = resolvePath(newpath, new_dfd);
  auto new_f = _graph.getArtifact(new_path);

  // We may delete the input file
  old_f->mayDelete(_command);

  // Unless the noreplace flag was set, we may delete the output file
  if ((flags & RENAME_NOREPLACE) == 0) {
    new_f->mayDelete(_command);
  }

  // Finish the syscall and resume
  int rc = finishSyscall();
  resume();

  // If the syscall failed, do nothing
  if (rc == -1) return;

  // We effectively read the old file
  old_f->readBy(_command);

  // We deleted the new file
  new_f->deletedBy(_command);
  _graph.unlinkArtifact(new_path);

  // Then link the old file into place
  _graph.linkArtifact(new_path, old_f);
  old_f->updatePath(new_path);

  // And we've written that file
  old_f->writtenBy(_command);
}

/////////////////

void Tracer::handleClone(shared_ptr<Process> p, int flags) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new thread id and then resume execution
  pid_t new_pid = p->getEventMessage();
  p->resume();

  // TODO: Handle flags

  // Threads in the same process just appear as pid references to the same process
  _processes[new_pid] = _processes[p->_pid];
}

void Tracer::handleFork(shared_ptr<Process> p) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new process id and resume execution
  pid_t new_pid = p->getEventMessage();
  p->resume();

  // If the call failed, do nothing
  if (new_pid == -1) return;

  // Create a new process running the same command
  auto new_proc = make_shared<Process>(_graph, new_pid, p->_cwd, p->_command, p->_fds);
  _processes[new_pid] = new_proc;
}

void Tracer::handleExit(shared_ptr<Process> p) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Remove the process. No need to resume, since the process has exited
  _processes.erase(p->_pid);
}

void Tracer::handleSyscall(shared_ptr<Process> p) {
  auto regs = p->getRegisters();

  // This giant switch statement invokes the appropriate system call handler on a traced
  // process after decoding the syscall arguments.
  switch (regs.SYSCALL_NUMBER) {
    case __NR_read:
      p->_read(regs.SYSCALL_ARG1);
      break;

    case __NR_write:
      p->_write(regs.SYSCALL_ARG1);
      break;

    case __NR_open:
      p->_open(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_close:
      p->_close(regs.SYSCALL_ARG1);
      break;

    case __NR_mmap:
      p->_mmap((void*)regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3, regs.SYSCALL_ARG4,
               regs.SYSCALL_ARG5, regs.SYSCALL_ARG6);
      break;

    case __NR_pread64:
      p->_pread64(regs.SYSCALL_ARG1);
      break;

    case __NR_pwrite64:
      p->_pwrite64(regs.SYSCALL_ARG1);
      break;

    case __NR_readv:
      p->_readv(regs.SYSCALL_ARG1);
      break;

    case __NR_writev:
      p->_writev(regs.SYSCALL_ARG1);
      break;

    case __NR_pipe:
      p->_pipe((int*)regs.SYSCALL_ARG1);
      break;

    case __NR_dup:
      p->_dup(regs.SYSCALL_ARG1);
      break;

    case __NR_dup2:
      p->_dup2(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_sendfile:
      p->_sendfile(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_fcntl:
      p->_fcntl(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_truncate:
      p->_truncate(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_ftruncate:
      p->_ftruncate(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_getdents:
      p->_getdents(regs.SYSCALL_ARG1);
      break;

    case __NR_chdir:
      p->_chdir(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fchdir:
      p->_fchdir(regs.SYSCALL_ARG1);
      break;

    case __NR_rename:
      p->_rename(p->readString(regs.SYSCALL_ARG1), p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_mkdir:
      p->_mkdir(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_rmdir:
      p->_rmdir(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_creat:
      p->_creat(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_unlink:
      p->_unlink(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_symlink:
      p->_symlink(p->readString(regs.SYSCALL_ARG1), p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_readlink:
      p->_readlink(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_chmod:
      p->_chmod(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_fchmod:
      p->_fchmod(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_chown:
      p->_chown(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_fchown:
      p->_fchown(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_lchown:
      p->_lchown(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_mknod:
      p->_mknod(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_chroot:
      p->_chroot(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_setxattr:
      p->_setxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_lsetxattr:
      p->_lsetxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fsetxattr:
      p->_fsetxattr(regs.SYSCALL_ARG1);
      break;

    case __NR_getxattr:
      p->_getxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_lgetxattr:
      p->_lgetxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fgetxattr:
      p->_fgetxattr(regs.SYSCALL_ARG1);
      break;

    case __NR_listxattr:
      p->_listxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_llistxattr:
      p->_llistxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_flistxattr:
      p->_flistxattr(regs.SYSCALL_ARG1);
      break;

    case __NR_removexattr:
      p->_removexattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_lremovexattr:
      p->_lremovexattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fremovexattr:
      p->_fremovexattr(regs.SYSCALL_ARG1);
      break;

    case __NR_getdents64:
      p->_getdents64(regs.SYSCALL_ARG1);
      break;

    case __NR_openat:
      p->_openat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                 regs.SYSCALL_ARG4);
      break;

    case __NR_mkdirat:
      p->_mkdirat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3);
      break;

    case __NR_mknodat:
      p->_mknodat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                  regs.SYSCALL_ARG4);
      break;

    case __NR_fchownat:
      p->_fchownat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                   regs.SYSCALL_ARG4, regs.SYSCALL_ARG5);
      break;

    case __NR_unlinkat:
      p->_unlinkat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3);
      break;

    case __NR_renameat:
      p->_renameat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                   p->readString(regs.SYSCALL_ARG4));
      break;

    case __NR_symlinkat:
      p->_symlinkat(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2,
                    p->readString(regs.SYSCALL_ARG3));
      break;

    case __NR_readlinkat:
      p->_readlinkat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_fchmodat:
      p->_fchmodat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                   regs.SYSCALL_ARG4);
      break;

    case __NR_splice:
      p->_splice(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3, regs.SYSCALL_ARG4);
      break;

    case __NR_tee:
      p->_tee(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2 /*, regs.SYSCALL_ARG3*/);  // Omitting length
      break;

    case __NR_vmsplice:
      p->_vmsplice(regs.SYSCALL_ARG1);
      break;

    case __NR_dup3:
      p->_dup3(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_pipe2:
      p->_pipe2((int*)regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_preadv:
      p->_preadv(regs.SYSCALL_ARG1);
      break;

    case __NR_pwritev:
      p->_pwritev(regs.SYSCALL_ARG1);
      break;

    case __NR_renameat2:
      p->_renameat2(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                    p->readString(regs.SYSCALL_ARG4), regs.SYSCALL_ARG5);
      break;

    case __NR_copy_file_range:
      p->_copy_file_range(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_preadv2:
      p->_preadv2(regs.SYSCALL_ARG1);
      break;

    case __NR_pwritev2:
      p->_pwritev2(regs.SYSCALL_ARG1);
      break;

    default:
      auto iter = syscalls.find(regs.SYSCALL_NUMBER);
      if (iter == syscalls.end()) {
        FAIL << "Unexpected system call number: " << regs.SYSCALL_NUMBER;
      } else {
        WARN << "Missing case for syscall: " << syscalls[regs.SYSCALL_NUMBER];
        p->resume();
      }
  }
}

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

path Tracer::Process::getExecutable() {
  char path_buffer[24];  // 24 is long enough for any integer PID
  sprintf(path_buffer, "/proc/%d/exe", _pid);

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

user_regs_struct Tracer::Process::getRegisters() {
  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, _pid, nullptr, &regs)) << "Failed to get registers: " << ERR;
  return regs;
}

string Tracer::Process::readString(uintptr_t tracee_pointer) {
  string output;

  // Loop to fetch words at a time until we find a null byte
  while (true) {
    // To properly check for errors when doing PEEKDATA, we need to clear then check errno,
    // since PEEKDATA could validly return -1.
    errno = 0;
    long data = ptrace(PTRACE_PEEKDATA, _pid, tracee_pointer + output.size(), nullptr);
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

uintptr_t Tracer::Process::readData(uintptr_t tracee_pointer) {
  // Clear errno so we can detect errors
  errno = 0;
  uintptr_t result = ptrace(PTRACE_PEEKDATA, _pid, tracee_pointer, nullptr);
  FAIL_IF(errno != 0) << "Failed to read data from traced process: " << ERR;
  return result;
}

// Launch a program fully set up with ptrace and seccomp to be traced by the current process.
// launch_traced will return the PID of the newly created process, which should be running (or at
// least ready to be waited on) upon return.
static pid_t launch_traced(char const* exec_path, char* const argv[],
                           vector<InitialFdEntry> initial_fds) {
  // In terms of overall structure, this is a bog standard fork/exec spawning function.
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

    vector<struct sock_filter> filter;

    // Load the syscall number
    filter.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)));

    // Loop over syscalls
    for (auto& entry : syscalls) {
      uint32_t syscall_nr = entry.first;
      // Check if the syscall matches the current entry
      filter.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, syscall_nr, 0, 1));

      // On a match, return trace
      filter.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_TRACE));
    }

    // Default case allows the syscall
    filter.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW));

    struct sock_fprog bpf_program;
    bpf_program.filter = filter.data();
    bpf_program.len = filter.size();

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
