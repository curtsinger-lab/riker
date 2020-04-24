#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iterator>
#include <list>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include <fcntl.h>
#include <limits.h>
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

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "tracing/syscalls.hh"
#include "ui/log.hh"

using std::list;
using std::make_shared;
using std::pair;
using std::shared_ptr;
using std::string;
using std::unique_ptr;

static pid_t launch_traced(shared_ptr<Command> cmd);

void Tracer::run(shared_ptr<Command> cmd) {
  pid_t pid = launch_traced(cmd);

  _processes[pid] = make_shared<Process>(*this, pid, ".", cmd, cmd->getInitialFDs());

  // Sometime we get tracing events before we can process them. This queue holds that list
  list<pair<pid_t, int>> event_queue;

  while (true) {
    int wait_status;
    pid_t child;

    // Do we have an event to process?
    bool have_event = false;

    // Check if any queued events are ready to be processed
    for (auto iter = event_queue.begin(); iter != event_queue.end(); iter++) {
      // If the current entry's pid is now a known process, process it
      if (_processes.find(iter->first) != _processes.end()) {
        // Pull out the child and status values
        child = iter->first;
        wait_status = iter->second;

        // Drop the event from the queue
        event_queue.erase(iter);

        // We have an event now
        have_event = true;
        break;
      }
    }

    // If we didn't pull an event from the queue, wait for one
    while (!have_event) {
      child = wait(&wait_status);

      // Handle errors
      if (child == -1) {
        // If errno is ECHILD, we're done and can return
        if (errno == ECHILD)
          return;
        else
          FAIL << "Error while waiting: " << ERR;
      }

      // Does this event refer to a process we don't know about yet?
      if (_processes.find(child) == _processes.end()) {
        // Yes. Queue the event so we can try another one.
        event_queue.emplace_back(child, wait_status);
      } else {
        // No. The event is for a known process, so we'll deal with it right now
        have_event = true;
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
        // TODO: Is this called in the child just after fork()?
        handleFork(p);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // TODO: Is this called before or after exec?
        FAIL << "handleExec is gone. I thought we wouldn't need it?";

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        // TODO: Is this called in the child just after clone()?
        auto regs = p->getRegisters();
        handleClone(p, regs.SYSCALL_ARG1);

      } else {
        // The traced process received a signal. Just pass it along.
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }

    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
      // Stopped on exit
      handleExit(p);
    }
  }
}

void Tracer::finalize() {
  // Save fingerprints and metadata for the final versions of all remaining artifacts
  for (auto& [_, artifact] : _artifacts) {
    artifact->getLatestVersion().saveMetadata();
    artifact->getLatestVersion().saveFingerprint();
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

path Tracer::Process::resolvePath(path p, int at) {
  // TODO: Handle chroot-ed processes correctly

  // We're going to build a full path from the reference. Simplest case is an absolute path.
  path full_path = p;

  // Relative paths have to be relative to something
  if (p.is_relative()) {
    // By default, paths are relative to the current directory
    path base = _cwd;

    // But if the file is not relative to cwd, get the path for the specified base
    if (at != AT_FDCWD) {
      base = _fds.at(at).getArtifact()->getPath();
    }

    full_path = base / p;
  }

  // Normalize path
  return full_path.lexically_normal();
}

shared_ptr<Artifact> Tracer::getArtifact(path p, bool follow_links) {
  // Now that we have a path, we can stat it
  struct stat statbuf;
  int rc;
  if (follow_links)
    rc = stat(p.c_str(), &statbuf);
  else
    rc = lstat(p.c_str(), &statbuf);

  // If stat failed, there is no artifact to resolve to. Return a null pointer
  if (rc) return shared_ptr<Artifact>();

  // Check for an existing inode entry
  auto iter = _artifacts.find(statbuf.st_ino);
  if (iter != _artifacts.end()) {
    // Found. Return it.
    return iter->second;
  }

  // No existing artifact found. Create a new one.
  shared_ptr<Artifact> result = make_shared<Artifact>(p);

  // Add the artifact to the map
  _artifacts.emplace(statbuf.st_ino, result);

  // All done
  return result;
}

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

// Some system calls are handled as aliases for these. See inline definitions in Tracer.hh.

void Tracer::Process::_read(int fd) {
  // Get the descriptor
  auto& descriptor = _fds.at(fd);

  // Get the reference used to read
  auto ref = descriptor.getReference();

  // Get the artifact being read
  auto artifact = descriptor.getArtifact();

  // The current command depends on the contents of this file
  _command->contentsMatch(ref, descriptor.getArtifact());

  // We can't wait for the syscall to finish here because of this scenario:
  //  fd may be the read end of a pipe that is currently empty. The process that will write to the
  //  pipe is also blocked, but we're not handling it now. In that case, the syscall will not
  //  finish until we resume the *other* process. To handle this case correctly we'd need to place
  //  a wait for any child after resuming the blocked process. pre_ and post_ hooks for syscalls
  //  would work, but we don't always need them. Threads would also work, btu that creates other
  //  problems.
  resume();
}

void Tracer::Process::_write(int fd) {
  // Get the descriptor
  auto descriptor = _fds.at(fd);

  // Get the reference used to write
  auto ref = descriptor.getReference();

  // Get the artifact being written
  auto artifact = descriptor.getArtifact();

  // Record our dependency on the old contents of the artifact
  _command->contentsMatch(ref, artifact);

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  // If the write syscall failed, there's no need to log a write
  if (rc == -1) return;

  // Record the update to the artifact contents
  _command->setContents(ref, artifact);
}

void Tracer::Process::_close(int fd) {
  // NOTE: We assume close calls always succeed. Erasing a non-existent file descriptor is harmless

  // Resume the process
  resume();

  // Remove the file descriptor
  _fds.erase(fd);
}

void Tracer::Process::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) {
  // Skip anonymous mappings. We never need to handle these because they only allow communication
  // within a single command.
  if (fd == -1) {
    resume();
    return;
  }

  // Run the syscall to find out if the mmap succeeded
  void* rc = (void*)finishSyscall();

  // If the map failed there's nothing to log
  if (rc == MAP_FAILED) {
    resume();
    return;
  }

  // Get the descriptor from the fd number
  auto descriptor = _fds.at(fd);

  // Get the reference for the file we just mapped
  auto ref = descriptor.getReference();

  // And get the artifact referenced
  auto artifact = descriptor.getArtifact();

  // By mmapping a file, the command implicitly depends on its contents at the time of
  // mapping.
  _command->contentsMatch(ref, artifact);

  // If the mapping is writable, and the file was opened in write mode, the command
  // is also effectively setting the contents of the file.
  bool writable = (prot & PROT_WRITE) && descriptor.isWritable();
  if (writable) {
    _command->setContents(ref, artifact);
  }

  // TODO: we need to track which commands have a given artifact mapped.
  // Any time that artifact is modified, all commands that have it mapped will get an
  // implicit CONTENTS_MATCH line added because they could see the new version.
  // Also, any commands with writable mappings of a file could be setting the contents
  // of the file at any time.
  // Any artifact with multiple mappers, at least one of whom has a writable mapping,
  // creates a cycle. All commands involved in that cycle must be collapsed.

  // Resume the process here, because the command *could* immediately write to the file.
  // We may have needed to take a fingerprint of the old, unwritten version, so we can't
  // resume immediately after a writable mapping.
  resume();
}

int Tracer::Process::_dup(int fd) {
  // Finish the syscall to get the new file descriptor, then resume the process
  int newfd = finishSyscall();
  resume();

  // If the syscall failed, do nothing
  if (newfd == -1) return newfd;

  // Add the new entry for the duped fd
  _fds.erase(newfd);
  _fds.emplace(newfd, _fds.at(fd));

  // Duped fds do not inherit the cloexec flag
  _fds.at(newfd).setCloexec(false);

  // Return the new fd. This is helpful for handling some of the fcntl variants
  return newfd;
}

void Tracer::Process::_sendfile(int out_fd, int in_fd) {
  WARN << "sendfile syscall is not updated";
  resume();
  /*
    // As with _write above, we may have to fingerprint the output file, although we won't know
    until
    // after the syscall (it could fail).
    auto in_f = _fds[in_fd].getRef()->getArtifact();
    auto out_f = _fds[out_fd].getRef()->getArtifact();

    // Take a fingerprint if we need one
    out_f->mayWrite(_command);

    // Finish the system call and resume
    int rc = finishSyscall();
    resume();

    // If the syscall failed, do nothing
    if (rc == -1) return;

    in_f->readBy(_command);
    out_f->writtenBy(_command);*/
}

void Tracer::Process::_faccessat(int dirfd, string pathname, int mode, int flags) {
  // Generate a normalized absolute path from pathname and dirfd
  auto p = resolvePath(pathname, dirfd);

  // Record the command's access to this path with the given flags
  auto ref = _command->access(p, AccessFlags::fromAccess(mode, flags));

  // Finish the syscall so we can see its result
  int rc = finishSyscall();

  // Resume the process' execution
  resume();

  // Did the access() call succeed?
  if (rc == 0) {
    _command->isOK(ref);
  } else {
    // Record the error. We negate the return code because syscalls always return negative errors
    _command->isError(ref, -rc);
  }
}

void Tracer::Process::_fstatat(int dirfd, string pathname, int flags) {
  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if ((flags & AT_EMPTY_PATH) == AT_EMPTY_PATH) {
    // This is essentially an fstat call
    auto descriptor = _fds.at(dirfd);
    auto ref = descriptor.getReference();
    auto artifact = descriptor.getArtifact();

    // Record the dependency on metadata
    _command->metadataMatch(ref, artifact);

  } else {
    // This is a regular stat call (with an optional base directory descriptor)
    auto p = resolvePath(pathname, dirfd);

    // Create the reference
    auto ref = _command->access(p, {});

    // Finish the syscall to see if the reference succeeds
    int rc = finishSyscall();

    // Log the success or failure
    if (rc == 0) {
      _command->isOK(ref);

      // Get the artifact that was stat-ed
      auto artifact = _tracer.getArtifact(p);

      // Record the dependence on the artifact's metadata
      _command->metadataMatch(ref, artifact);
    } else {
      // Record the error. Negate rc because syscalls return negative errors
      _command->isError(ref, -rc);
    }
  }

  resume();
}

void Tracer::Process::_execveat(int dfd, string filename) {
  // Get the path to the executable we will exec
  auto exe_path = resolvePath(filename, dfd);

  // The command accesses this path with execute permissions
  auto exe_ref = _command->access(exe_path, {.x = true});

  // Finish the exec syscall
  int rc = finishSyscall();

  // Not sure why, but exec returns -38 on success.
  // If we see something else, handle the error
  if (rc != -38) {
    // Failure! Record a failed reference. Negate rc because syscalls return negative errors
    _command->isError(exe_ref, -rc);

    // Resume the process and stop handling
    resume();
    return;
  }

  // If we reached this point, the executable reference was okay
  _command->isOK(exe_ref);

  // Get the registers so we can read exec arguments
  auto regs = getRegisters();

  // Read the args array
  vector<string> args;
  int child_argc = readData(regs.rsp);
  for (int i = 0; i < child_argc; i++) {
    uintptr_t arg_ptr = readData(regs.rsp + (1 + i) * sizeof(long));
    args.push_back(readString(arg_ptr));
  }

  // Resume the child
  resume();

  // Erase any cloexec fds from the process file descriptor table
  list<int> to_erase;
  for (auto& entry : _fds) {
    if (entry.second.isCloexec()) {
      to_erase.push_back(entry.first);
    }
  }
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // This process launches a new command, and is now running that command
  _command = _command->launch(exe_path, args, _fds);

  // Get the executable file artifact
  auto exe_artifact = _tracer.getArtifact(exe_path, true);

  // The child command reads the contents of the executable file
  auto child_exe_ref = _command->access(exe_path, {.r = true});

  // The reference to the executable file must succeed
  _command->isOK(child_exe_ref);

  // We also depend on the contents of the executable file at this point
  _command->contentsMatch(child_exe_ref, exe_artifact);

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes that
  // participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}

void Tracer::Process::_fcntl(int fd, int cmd, unsigned long arg) {
  if (cmd == F_DUPFD) {
    // Handle fcntl(F_DUPFD) as a dup call. The return value is the new fd.
    _dup(fd);  // _dup will resume the process and return the new fd to us

  } else if (cmd == F_DUPFD_CLOEXEC) {
    // fcntl(F_DUPFD_CLOEXEC) is just like a dup call, followed by setting cloexec to true
    int newfd = _dup(fd);  // _dup will resume the process and return the new fd to us
    _fds.at(newfd).setCloexec(true);

  } else if (cmd == F_SETFD) {
    resume();
    // Set the cloexec flag using the argument flags
    _fds.at(fd).setCloexec(arg & FD_CLOEXEC);

  } else {
    // Some other operation we do not need to handle
    // TODO: Filter these stops out with BPF/seccomp
    resume();
  }
}

void Tracer::Process::_truncate(string pathname, long length) {
  WARN << "truncate syscall is not updated";
  resume();
  /*
    // Get the file
    auto p = resolvePath(pathname);
    auto f = _tracer.getArtifact(p);

    // Notify the file of an upcoming change
    if (length == 0) {
      f->mayTruncate(_command);
    } else {
      f->mayWrite(_command);
    }

    // Record the reference
    _command->addReference(p);

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
    }*/
}

void Tracer::Process::_ftruncate(int fd, long length) {
  WARN << "ftruncate syscall is not updated";
  resume();
  /*
    auto f = _fds[fd].getRef()->getArtifact();

    if (length == 0) {
      f->truncatedBy(_command);
    } else {
      f->writtenBy(_command);
    }

    // Resume after logging so we have a chance to fingerprint
    resume();*/
}

void Tracer::Process::_chdir(string filename) {
  int rc = finishSyscall();

  // Update the current working directory if the chdir call succeeded
  if (rc == 0) {
    _cwd = resolvePath(filename);
  }

  resume();
}

void Tracer::Process::_fchdir(int fd) {
  WARN << "fchdir syscall is not updated";
  resume();
  /*
    int rc = finishSyscall();
    resume();

    if (rc == 0) {
      auto f = _fds[fd].getRef()->getArtifact();
      WARN_IF(!f) << "Unable to locate file used in fchdir";
      _cwd = f->getPath();
    }
  */
}

void Tracer::Process::_lchown(string filename, uid_t user, gid_t group) {
  WARN << "lchown syscall is not updated";
  resume();
  /*
    // Resolve the path without following links, then get the file tracking object
    auto p = resolvePath(filename);
    auto f = _tracer.getArtifact(p, false);  // Do not follow links

    // Indicate that we may write this file
    f->mayWrite(_command);

    // Record the reference
    _command->addReference(p, {.nofollow = true});

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    // If the syscall failed, bail out
    if (rc == -1) return;

    // Record a write
    f->writtenBy(_command);
  */
}

void Tracer::Process::_chroot(string filename) {
  WARN << "chroot is not updated";
  resume();
  /*
    auto p = resolvePath(filename);
    auto f = _tracer.getArtifact(p);
    string newroot = p;

    // Record the reference
    _command->addReference(p);

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) {
      // Update the process root
      _root = newroot;

      // A directory must exist to
      f->readBy(_command);
    }
  */
}

void Tracer::Process::_setxattr(string pathname) {
  WARN << "setxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    auto p = resolvePath(pathname);
    auto f = _tracer.getArtifact(p);

    // Notify the file that it may be written
    f->mayWrite(_command);

    // Record the reference
    _command->addReference(p);

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) f->writtenBy(_command);
  */
}

void Tracer::Process::_lsetxattr(string pathname) {
  WARN << "lsetxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    // Same as setxattr, except we do not follow links
    auto p = resolvePath(pathname);
    auto f = _tracer.getArtifact(pathname, false);  // Do not follow links

    // Notify the file that it may be written
    f->mayWrite(_command);

    // Record the reference
    _command->addReference(p, {.nofollow = true});

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) f->writtenBy(_command);
  */
}

void Tracer::Process::_getxattr(string pathname) {
  WARN << "getxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    auto p = resolvePath(pathname);
    auto f = _tracer.getArtifact(p);

    // Record the reference
    _command->addReference(p);

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) f->readBy(_command);
  */
}

void Tracer::Process::_lgetxattr(string pathname) {
  WARN << "lgetxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    // Same as getxattr, except we don't follow links
    auto p = resolvePath(pathname);
    auto f = _tracer.getArtifact(pathname, false);  // Do not follow links

    // Record the reference
    _command->addReference(p, {.nofollow = true});

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) f->readBy(_command);
  */
}

void Tracer::Process::_openat(int dfd, string filename, int flags, mode_t mode) {
  // Convert the path to an absolute, normalized lexical form
  auto p = resolvePath(filename, dfd);

  // This reference may resolve to an existing artifact, and if the O_TRUNC flag is set, could
  // modify the artifact directly. Try to resolve the path now.
  auto artifact = _tracer.getArtifact(p, (flags & O_NOFOLLOW) == O_NOFOLLOW);

  // The command makes a reference to a path, possibly modifying artifact f
  auto ref_flags = AccessFlags::fromOpen(flags);
  auto ref = _command->access(p, ref_flags);

  // Allow the syscall to finish, and record the result
  int fd = finishSyscall();

  // Let the process continue
  resume();

  // Check whether the openat call succeeded or failed
  if (fd >= 0) {
    bool created = false;
    // If the artifact did not already exist, but the syscall succeeded, there is now an artifact
    // we can resolve to. Get it.
    if (!artifact) {
      created = true;
      artifact = _tracer.getArtifact(p, (flags & O_NOFOLLOW) == O_NOFOLLOW);
    }

    // The command observed a successful openat, so add this predicate to the command log
    _command->isOK(ref);

    // Handle O_CREAT and O_TRUNC
    if (created && (flags & O_CREAT)) {
      // We created a file, so tag a new (empty) version
      _command->setContents(ref, artifact);
    } else if (flags & O_TRUNC) {
      // We truncated a file, so tag a new (empty) version
      _command->setContents(ref, artifact);
    }

    // Is this new descriptor closed on exec?
    bool cloexec = ((flags & O_CLOEXEC) == O_CLOEXEC);

    // Record the reference in the correct location in this process' file descriptor table
    _fds.emplace(fd, FileDescriptor(ref, artifact, ref_flags.w, cloexec));

  } else {
    // The command observed a failed openat, so add the error predicate to the command log
    // Negate fd because syscalls return negative errors
    _command->isError(ref, -fd);
  }
}

void Tracer::Process::_mkdirat(int dfd, string pathname, mode_t mode) {
  WARN << "mkdirat syscall is not updated";
  resume();
  /*
    auto p = resolvePath(pathname, dfd);
    auto f = _tracer.getArtifact(p);
    bool dir_existed = f != nullptr;

    // Record the reference
    // TODO: is this a creat or excl reference? Need to look at result of syscall
    _command->addReference(p);

    // Run the syscall
    int rc = finishSyscall();
    resume();

    // If the call failed, do nothing
    if (rc) return;

    if (!dir_existed) {
      f = _tracer.getArtifact(p);
      f->createdBy(_command);
    }
  */

  // TODO: if creation failed, does this command now depend on the directory that already exists?
}

void Tracer::Process::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) {
  WARN << "mknodat syscall is not updated";
  resume();
  /*
    // TODO: What kind of node is this? Need to handle device, files, FIFOs, etc.
    // TODO: Probably also need to set creat/excl flags in reference

    int rc = finishSyscall();
    resume();

    // Give up if the syscall fails
    if (rc != 0) return;

    auto p = resolvePath(filename, dfd);
    auto f = _tracer.getArtifact(p);

    // Record the reference
    _command->addReference(filename);

    f->createdBy(_command);
  */
}

void Tracer::Process::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flags) {
  WARN << "fchownat syscall is not updated";
  resume();
  /*
    shared_ptr<Artifact> f;

    // An empty path means just use dfd as the file
    if (flags & AT_EMPTY_PATH) {
      f = _fds[dfd].getRef()->getArtifact();
    } else {
      // Are we following links or not?
      bool follow_links = (flags & AT_SYMLINK_NOFOLLOW) == 0;

      // Resolve the path, then get the file tracking object
      auto p = resolvePath(filename, dfd);
      f = _tracer.getArtifact(p, follow_links);

      // Record the reference
      _command->addReference(p, Ref::Flags::fromChown(flags));
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
  */
}

void Tracer::Process::_unlinkat(int dfd, string pathname, int flags) {
  WARN << "unlinkat syscall is not updated";
  resume();
  /*
    auto p = resolvePath(pathname, dfd);
    auto f = _tracer.getArtifact(p);

    // Record the reference
    _command->addReference(p, Ref::Flags::fromUnlink(flags));

    f->mayDelete(_command);

    int rc = finishSyscall();
    resume();

    if (rc == 0) f->deletedBy(_command);
  */
}

void Tracer::Process::_symlinkat(string oldname, int newdfd, string newname) {
  WARN << "symlinkat syscall is not updated";
  resume();
  /*
    // Creating a symlink doesn't actually do anything with the target (oldname)
    auto newp = resolvePath(newname, newdfd);

    // TODO: Set creat/excl for new link if this syscall succeeds? No, maybe we always set them,
    then
    // record reference failure if the syscall fails.
    _command->addReference(newp);

    resume();
    // TODO
  */
}

void Tracer::Process::_readlinkat(int dfd, string pathname) {
  WARN << "readlinkat syscall is not updated";
  resume();
  /*
    auto p = resolvePath(pathname, dfd);
    _command->addReference(p, {.nofollow = true});

    resume();
    // TODO
  */
}

void Tracer::Process::_fchmodat(int dfd, string filename, mode_t mode, int flags) {
  WARN << "fchmodat syscall is not updated";
  resume();
  /*
    // Find the file object
    auto p = resolvePath(filename, dfd);
    auto f = _tracer.getArtifact(p, (flags & AT_SYMLINK_NOFOLLOW) == 0);

    // Record the reference
    _command->addReference(p, Ref::Flags::fromChmod(flags));

    // Indicate that we may write this file
    f->mayWrite(_command);

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    // If the syscall failed, bail out
    if (rc != 0) return;

    // Record the write
    f->writtenBy(_command);
  */
}

void Tracer::Process::_tee(int fd_in, int fd_out) {
  WARN << "tee syscall is not updated";
  resume();
  /*
    auto input_f = _fds[fd_in].getRef()->getArtifact();
    auto output_f = _fds[fd_out].getRef()->getArtifact();

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
  */
}

void Tracer::Process::_dup3(int oldfd, int newfd, int flags) {
  // dup3 returns the new file descriptor, or error
  // Finish the syscall so we know what file descriptor to add to our table
  int rc = finishSyscall();
  resume();

  // If the syscall failed, we have nothing more to do
  // Note: this is different than a failed file access. This failure should not be affected
  //       by the state of the filesystem, so we don't have to log it.
  if (rc == -1) return;

  // Add the entry for the duped fd
  _fds.emplace(rc, _fds.at(oldfd));

  // If the flags include O_CLOEXEC, we have to set that property on the new file descriptor
  // If O_CLOEXEC is not set, any dup-ed fd is NOT cloexec
  _fds.at(rc).setCloexec((flags & O_CLOEXEC) == O_CLOEXEC);
}

void Tracer::Process::_pipe2(int* fds, int flags) {
  int rc = finishSyscall();

  // There is nothing to do if the syscall fails, but why would that ever happen?
  if (rc) {
    resume();
    return;
  }

  // Create a reference to the pipe
  auto ref = _command->pipe();

  // Read the file descriptors
  int read_pipefd = readData((uintptr_t)fds);
  int write_pipefd = readData((uintptr_t)fds + sizeof(int));

  // The command can continue
  resume();

  // Create a pipe artifact
  auto artifact = make_shared<Artifact>("pipe");

  // Check if this pipe is closed on exec
  bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

  // Fill in the file descriptor entries
  _fds.emplace(read_pipefd, FileDescriptor(ref, artifact, false, cloexec));
  _fds.emplace(write_pipefd, FileDescriptor(ref, artifact, true, cloexec));
}

void Tracer::Process::_renameat2(int old_dfd, string oldpath, int new_dfd, string newpath,
                                 int flags) {
  WARN << "renameat2 syscall is not updated";
  resume();
  /*
    string old_path = resolvePath(oldpath, old_dfd);
    auto old_f = _tracer.getArtifact(old_path);

    // Record the reference to the old file
    // TODO: Deal with flags
    _command->addReference(old_path);

    string new_path = resolvePath(newpath, new_dfd);
    auto new_f = _tracer.getArtifact(new_path);

    // Record the reference to the new file
    // TODO: Deal with flags
    _command->addReference(new_path);

    // We may delete the input file
    if (old_f) old_f->mayDelete(_command);

    // Unless the noreplace flag was set, we may delete the output file
    if (new_f && (flags & RENAME_NOREPLACE) == 0) {
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
    if (new_f) new_f->deletedBy(_command);

    // Then link the old file into place
    old_f->updatePath(new_path);

    // And we've written that file
    old_f->writtenBy(_command);
  */
}

void Tracer::Process::_lseek(int fd, off_t offset, int whence) {
  // TODO: track file descriptor offsets
  resume();
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

  LOG << "fork called in " << p;

  // Create a new process running the same command
  auto new_proc = make_shared<Process>(*this, new_pid, p->_cwd, p->_command, p->_fds);
  _processes[new_pid] = new_proc;

  LOG << "new process " << new_proc;
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
    case __NR_execve:
      p->_execve(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_execveat:
      p->_execveat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_stat:
      p->_stat(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_newfstatat:
      p->_fstatat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG4);
      break;

    case __NR_fstat:
      p->_fstat(regs.SYSCALL_ARG1);
      break;

    case __NR_lstat:
      p->_lstat(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_access:
      p->_access(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_faccessat:
      p->_faccessat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                    regs.SYSCALL_ARG4);
      break;

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

    case __NR_lseek:
      p->_lseek(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
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
static pid_t launch_traced(shared_ptr<Command> cmd) {
  // TODO: Fill these in
  vector<InitialFdEntry> initial_fds;

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

    vector<const char*> args;
    for (auto& s : cmd->getArguments()) {
      args.push_back(s.c_str());
    }

    // TODO: explicitly handle the environment
    execv(cmd->getExecutable().c_str(), (char* const*)args.data());

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

  // Handle a stop on entry to the exec
  waitpid(child_pid, &wstatus, 0);
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8)))
      << "Unexpected stop from child. Expected SECCOMP";

  // Let the child continue with its exec
  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  // Handle a stop from inside the exec
  waitpid(child_pid, &wstatus, 0);
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8)))
      << "Unexpected stop from child. Expected EXEC";

  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  return child_pid;
}
