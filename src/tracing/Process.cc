#include "Process.hh"

#include <memory>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>

#include "build/Artifact.hh"
#include "build/Build.hh"
#include "data/Command.hh"
#include "data/IR.hh"
#include "data/InitialFD.hh"
#include "tracing/FDEntry.hh"
#include "tracing/syscalls.hh"
#include "util/log.hh"

using std::dynamic_pointer_cast;
using std::make_shared;

namespace fs = std::filesystem;

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

user_regs_struct Process::getRegisters() {
  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, _pid, nullptr, &regs)) << "Failed to get registers: " << ERR;
  return regs;
}

void Process::resume() {
  FAIL_IF(ptrace(PTRACE_CONT, _pid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

long Process::finishSyscall() {
  FAIL_IF(ptrace(PTRACE_SYSCALL, _pid, nullptr, 0)) << "Failed to finish syscall: " << ERR;
  FAIL_IF(waitpid(_pid, nullptr, 0) != _pid) << "Unexpected child process stop";

  // Clear errno so we can check for errors
  errno = 0;
  long result = ptrace(PTRACE_PEEKUSER, _pid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  return result;
}

unsigned long Process::getEventMessage() {
  // Get the id of the new process
  unsigned long message;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, _pid, nullptr, &message))
      << "Unable to read ptrace event message: " << ERR;
  return message;
}

fs::path Process::resolvePath(fs::path p, int at) {
  // TODO: Handle chroot-ed processes correctly

  // We're going to build a full path from the reference. Simplest case is an absolute path.
  fs::path full_path = p;

  // Relative paths have to be relative to something
  if (p.is_relative()) {
    // By default, paths are relative to the current directory
    fs::path base = _cwd;

    // But if the file is not relative to cwd, get the path for the specified base
    if (at != AT_FDCWD) {
      // Get the file descriptor that serves as the base directory
      auto base_fd = _fds.at(at);

      // Attempt to cast the reference this descriptor was created with to an Access
      auto a = dynamic_pointer_cast<Access>(base_fd.getReference());

      FAIL_IF(!a) << "Attempted to resolve a path relative to an anonymous reference";

      base = a->getPath();
    }

    full_path = base / p;
  }

  // Normalize path
  return full_path.lexically_normal();
}

string Process::readString(uintptr_t tracee_pointer) {
  // Strings are just char arrays terminated by '\0'
  auto data = readTerminatedArray<char, '\0'>(tracee_pointer);

  // Convert the result to a string
  return string(data.begin(), data.end());
}

// Read a value of type T from this process
template <typename T>
T Process::readData(uintptr_t tracee_pointer) {
  // Reserve space for the value we will read
  T result;

  // Set up iovec structs for the remote read and local write
  struct iovec local = {.iov_base = &result, .iov_len = sizeof(T)};
  struct iovec remote = {.iov_base = (void*)tracee_pointer, .iov_len = sizeof(T)};

  // Do the read
  auto rc = process_vm_readv(_pid, &local, 1, &remote, 1, 0);

  // Check the result
  FAIL_IF(rc != sizeof(T)) << "Failed to read data from traced process";

  return result;
}

// Read an array of values up to a terminating value
template <typename T, T Terminator, size_t BatchSize>
vector<T> Process::readTerminatedArray(uintptr_t tracee_pointer) {
  // We will read BatchSize values at a time into this buffer
  T buffer[BatchSize];

  // As we go, we'll build the vector of values we read
  vector<T> result;

  // Keep track of our position in the remote array
  size_t position = 0;

  while (true) {
    // Set up iovecs to read from the array into buffer
    struct iovec local = {.iov_base = buffer, .iov_len = sizeof(buffer)};
    struct iovec remote = {.iov_base = (T*)tracee_pointer + position, .iov_len = sizeof(buffer)};

    // Do the read. The result is the number of bytes read, or -1 on failure.
    auto rc = process_vm_readv(_pid, &local, 1, &remote, 1, 0);

    // Check for failure
    FAIL_IF(rc == -1) << "Failed to read data from traced process: " << ERR;

    // Our position in the remote array is advanced by the number of bytes read. This will usually
    // be BatchSize, but reading can end early when we hit the end of a page/region
    position += rc;

    // Let the result vector know we're about to append a bunch of data
    result.reserve(result.size() + rc / sizeof(T));

    // Scan for a terminator
    for (size_t i = 0; i < rc / sizeof(T); i++) {
      // If we find a termiantor, it's time to return
      if (buffer[i] == Terminator) {
        // Insert all elements from buffer up to (but not including) the terminator
        result.insert(result.end(), buffer, buffer + i);
        return result;
      }
    }

    // No terminator found. We'll do another round of reading.

    // Copy all elements from buffer into the result vector
    result.insert(result.end(), buffer, buffer + BatchSize);
  }
}

vector<string> Process::readArgvArray(uintptr_t tracee_pointer) {
  auto arg_pointers = readTerminatedArray<uintptr_t, 0>(tracee_pointer);

  vector<string> args;
  for (auto arg_ptr : arg_pointers) {
    args.push_back(readString(arg_ptr));
  }
  return args;
}

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

// Some system calls are handled as aliases for these. See inline definitions in Tracer.hh.

void Process::_read(int fd) {
  // Get the descriptor
  auto& descriptor = _fds.at(fd);

  // Get the reference used to read
  auto ref = descriptor.getReference();

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

void Process::_write(int fd) {
  // Get the descriptor
  auto descriptor = _fds.at(fd);

  // Get the reference used to write
  auto ref = descriptor.getReference();

  // Record our dependency on the old contents of the artifact
  _command->contentsMatch(ref, descriptor.getArtifact());

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  // If the write syscall failed, there's no need to log a write
  if (rc == -1) return;

  // Record the update to the artifact contents
  _command->setContents(ref, descriptor.getArtifact());
}

void Process::_close(int fd) {
  // NOTE: We assume close calls always succeed. Erasing a non-existent file descriptor is harmless

  // Resume the process
  resume();

  // Remove the file descriptor
  _fds.erase(fd);
}

void Process::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) {
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

  // By mmapping a file, the command implicitly depends on its contents at the time of
  // mapping.
  _command->contentsMatch(ref, descriptor.getArtifact());

  // If the mapping is writable, and the file was opened in write mode, the command
  // is also effectively setting the contents of the file.
  bool writable = (prot & PROT_WRITE) && descriptor.isWritable();
  if (writable) {
    _command->setContents(ref, descriptor.getArtifact());
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

int Process::_dup(int fd) {
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

void Process::_sendfile(int out_fd, int in_fd) {
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

void Process::_faccessat(int dirfd, string pathname, int mode, int flags) {
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
    _command->referenceResult(ref, SUCCESS);
  } else {
    // Record the error. We negate the return code because syscalls always return negative errors
    _command->referenceResult(ref, -rc);
  }
}

void Process::_fstatat(int dirfd, string pathname, int flags) {
  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if ((flags & AT_EMPTY_PATH) == AT_EMPTY_PATH) {
    // This is essentially an fstat call
    auto descriptor = _fds.at(dirfd);
    auto ref = descriptor.getReference();

    // Record the dependency on metadata
    _command->metadataMatch(ref, descriptor.getArtifact());

  } else {
    // This is a regular stat call (with an optional base directory descriptor)
    auto p = resolvePath(pathname, dirfd);

    // Create the reference
    // TODO: handle nofollow
    auto ref = _command->access(p, {});

    // Finish the syscall to see if the reference succeeds
    int rc = finishSyscall();

    // Log the success or failure
    if (rc == 0) {
      _command->referenceResult(ref, SUCCESS);

      // Get the artifact that was stat-ed
      auto [artifact, env_rc, created] = _build.getEnv().get(_command, ref);

      FAIL_IF(!artifact) << "Unable to locate artifact for stat-ed file";

      // Record the dependence on the artifact's metadata
      _command->metadataMatch(ref, artifact);
    } else {
      // Record the error. Negate rc because syscalls return negative errors
      _command->referenceResult(ref, -rc);
    }
  }

  resume();
}

void Process::_execveat(int dfd, string filename, vector<string> args, vector<string> env) {
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
    _command->referenceResult(exe_ref, -rc);

    // Resume the process and stop handling
    resume();
    return;
  }

  // If we reached this point, the executable reference was okay
  _command->referenceResult(exe_ref, SUCCESS);

  // Resume the child
  resume();

  // Build a map of the initial file descriptors for the child command
  // As we build this map, keep track of which file descriptors have to be erased from the process'
  // current map of file descriptors.
  map<int, InitialFD> initial_fds;
  list<int> to_erase;

  for (auto& [index, fd] : _fds) {
    if (fd.isCloexec()) {
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, InitialFD(fd.getReference(), fd.isWritable()));
    }
  }
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // This process launches a new command, and is now running that command
  _command = _command->launch(exe_path, args, initial_fds);

  // Get the executable file artifact
  auto [exe_artifact, exe_rc, _] = _build.getEnv().get(_command, exe_ref);

  FAIL_IF(!exe_artifact) << "Failed to locate artifact for executable file";

  // The child command reads the contents of the executable file
  auto child_exe_ref = _command->access(exe_path, {.r = true});

  // The reference to the executable file must succeed
  _command->referenceResult(child_exe_ref, SUCCESS);

  // We also depend on the contents of the executable file at this point
  _command->contentsMatch(child_exe_ref, exe_artifact);

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes that
  // participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}

void Process::_fcntl(int fd, int cmd, unsigned long arg) {
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

void Process::_truncate(string pathname, long length) {
  auto p = resolvePath(pathname);
  auto ref = _command->access(p, AccessFlags{.w = true});

  // Get the artifact that's being truncated
  auto [artifact, env_rc, _] = _build.getEnv().get(_command, ref);

  // If length is non-zero, we depend on the previous contents
  // This only applies if the artifact exists
  if (length > 0 && artifact) {
    _command->contentsMatch(ref, artifact);
  }

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  // Did the call succeed?
  if (rc == 0) {
    // Record the successful reference
    _command->referenceResult(ref, SUCCESS);

    // Make sure the artifact actually existed
    FAIL_IF(!artifact) << "Failed to get artifact for truncated file";

    // Record the update to the artifact contents
    _command->setContents(ref, artifact);

  } else {
    // Record the failed reference
    _command->referenceResult(ref, -rc);
  }
}

void Process::_ftruncate(int fd, long length) {
  // Get the descriptor
  auto descriptor = _fds.at(fd);

  // If length is non-zero, this is a write so we depend on the previous contents
  if (length > 0) {
    _command->contentsMatch(descriptor.getReference(), descriptor.getArtifact());
  }

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  if (rc == 0) {
    // Record the update to the artifact contents
    _command->setContents(descriptor.getReference(), descriptor.getArtifact());
  }
}

void Process::_chdir(string filename) {
  int rc = finishSyscall();
  resume();

  // Update the current working directory if the chdir call succeeded
  if (rc == 0) {
    _cwd = resolvePath(filename);
  }
}

void Process::_fchdir(int fd) {
  int rc = finishSyscall();
  resume();

  if (rc == 0) {
    // Get the path to the artifact this descriptor references
    auto descriptor = _fds.at(fd);
    auto ref = descriptor.getReference();
    auto a = dynamic_pointer_cast<Access>(ref);

    // Make sure there really is a path
    FAIL_IF(!a) << "fchdir to anonymous artifact succeeded";

    // Update the working directory
    _cwd = a->getPath();
  }
}

void Process::_lchown(string filename, uid_t user, gid_t group) {
  WARN << "lchown syscall is not updated";
  resume();
  /*
    // Resolve the path without following links, then get the file tracking object
    auto p = resolvePath(filename);
    auto f = _build.getEnv().get(p, false);  // Do not follow links

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

void Process::_chroot(string filename) {
  WARN << "chroot is not updated";
  resume();
  /*
    auto p = resolvePath(filename);
    auto f = _build.getEnv().get(p);
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

void Process::_setxattr(string pathname) {
  WARN << "setxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    auto p = resolvePath(pathname);
    auto f = _build.getEnv().get(p);

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

void Process::_lsetxattr(string pathname) {
  WARN << "lsetxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    // Same as setxattr, except we do not follow links
    auto p = resolvePath(pathname);
    auto f = _build.getEnv().get(pathname, false);  // Do not follow links

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

void Process::_getxattr(string pathname) {
  WARN << "getxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    auto p = resolvePath(pathname);
    auto f = _build.getEnv().get(p);

    // Record the reference
    _command->addReference(p);

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) f->readBy(_command);
  */
}

void Process::_lgetxattr(string pathname) {
  WARN << "lgetxattr syscall is not updated";
  resume();
  /*
    // Get the process and file
    // Same as getxattr, except we don't follow links
    auto p = resolvePath(pathname);
    auto f = _build.getEnv().get(pathname, false);  // Do not follow links

    // Record the reference
    _command->addReference(p, {.nofollow = true});

    // Finish the syscall and resume
    int rc = finishSyscall();
    resume();

    if (rc != -1) f->readBy(_command);
  */
}

void Process::_openat(int dfd, string filename, int flags, mode_t mode) {
  // Convert the path to an absolute, normalized lexical form
  auto p = resolvePath(filename, dfd);

  // The command makes a reference to a path, possibly modifying artifact f
  auto ref_flags = AccessFlags::fromOpen(flags, mode);
  auto ref = _command->access(p, ref_flags);

  // Attempt to get an artifact using this reference *BEFORE* running the syscall.
  // This will tell us whether or not the syscall created the artifact
  auto [artifact, _, created] = _build.getEnv().get(_command, ref);

  // Allow the syscall to finish, and record the result
  int fd = finishSyscall();

  // Let the process continue
  resume();

  // Check whether the openat call succeeded or failed
  if (fd >= 0) {
    // The command observed a successful openat, so add this predicate to the command log
    _command->referenceResult(ref, SUCCESS);

    FAIL_IF(!artifact) << "Failed to locate artifact for opened file";

    // If the file was created or truncated by the open call, set the contents in the artifact
    if (ref_flags.truncate) {
      _command->setContents(ref, artifact);
    }

    // Is this new descriptor closed on exec?
    bool cloexec = ((flags & O_CLOEXEC) == O_CLOEXEC);

    // Record the reference in the correct location in this process' file descriptor table
    _fds.emplace(fd, FDEntry(ref, artifact, ref_flags.w, cloexec));

  } else {
    // The command observed a failed openat, so add the error predicate to the command log
    // Negate fd because syscalls return negative errors
    _command->referenceResult(ref, -fd);
  }
}

void Process::_mkdirat(int dfd, string pathname, mode_t mode) {
  WARN << "mkdirat syscall is not updated";
  resume();
  /*
    auto p = resolvePath(pathname, dfd);
    auto f = _build.getEnv().get(p);
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
      f = _build.getEnv().get(p);
      f->createdBy(_command);
    }
  */

  // TODO: if creation failed, does this command now depend on the directory that already exists?
}

void Process::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) {
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
    auto f = _build.getEnv().get(p);

    // Record the reference
    _command->addReference(filename);

    f->createdBy(_command);
  */
}

void Process::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flags) {
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
      f = _build.getEnv().get(p, follow_links);

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

void Process::_unlinkat(int dfd, string pathname, int flags) {
  WARN << "unlinkat syscall is not updated";
  resume();
  /*
    auto p = resolvePath(pathname, dfd);
    auto f = _build.getEnv().get(p);

    // Record the reference
    _command->addReference(p, Ref::Flags::fromUnlink(flags));

    f->mayDelete(_command);

    int rc = finishSyscall();
    resume();

    if (rc == 0) f->deletedBy(_command);
  */
}

void Process::_symlinkat(string oldname, int newdfd, string newname) {
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

void Process::_readlinkat(int dfd, string pathname) {
  // We're making a reference to a symlink, so don't follow links
  auto p = resolvePath(pathname, dfd);
  auto ref = _command->access(p, {.nofollow = true});

  // Finish the syscall and then resume the process
  int rc = finishSyscall();
  resume();

  // Did the call succeed?
  if (rc >= 0) {
    // Yes. Record the successful reference
    _command->referenceResult(ref, SUCCESS);

    // Get the artifact that we referenced
    auto [artifact, _, __] = _build.getEnv().get(_command, ref);

    FAIL_IF(!artifact) << "Failed to get artifact for successfully-read link";

    // We depend on this artifact's contents now
    _command->contentsMatch(ref, artifact);

  } else {
    // No. Record the failure
    _command->referenceResult(ref, -rc);
  }
}

void Process::_fchmodat(int dfd, string filename, mode_t mode, int flags) {
  // Make a reference to the file that will be chmod-ed.
  // TODO: We need permissions in the directory to chmod, right?
  auto p = resolvePath(filename, dfd);
  auto ref = _command->access(p, AccessFlags{});

  // Get the artifact that we're going to chmod
  auto [artifact, _, __] = _build.getEnv().get(_command, ref);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (artifact) {
    _command->metadataMatch(ref, artifact);
  }

  // Finish the syscall and then resume the process
  int rc = finishSyscall();
  resume();

  // Did the call succeed?
  if (rc >= 0) {
    // Yes. Record the successful reference
    _command->referenceResult(ref, SUCCESS);

    FAIL_IF(!artifact) << "Failed to get artifact";

    // We've now set the artifact's metadata
    _command->setMetadata(ref, artifact);

  } else {
    // No. Record the failure
    _command->referenceResult(ref, -rc);
  }
}

void Process::_tee(int fd_in, int fd_out) {
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

void Process::_dup3(int oldfd, int newfd, int flags) {
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

void Process::_pipe2(int* fds, int flags) {
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

  // Get an artifact for this pipe
  auto [artifact, _, __] = _build.getEnv().get(_command, ref);

  FAIL_IF(!artifact) << "Failed to get artifact for pipe";

  // The command sets the contents of the pipe on creation
  _command->setContents(ref, artifact);

  // Check if this pipe is closed on exec
  bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

  // Fill in the file descriptor entries
  _fds.emplace(read_pipefd, FDEntry(ref, artifact, false, cloexec));
  _fds.emplace(write_pipefd, FDEntry(ref, artifact, true, cloexec));
}

void Process::_renameat2(int old_dfd, string oldpath, int new_dfd, string newpath, int flags) {
  WARN << "renameat2 syscall is not updated";
  resume();
  /*
    string old_path = resolvePath(oldpath, old_dfd);
    auto old_f = _build.getEnv().get(old_path);

    // Record the reference to the old file
    // TODO: Deal with flags
    _command->addReference(old_path);

    string new_path = resolvePath(newpath, new_dfd);
    auto new_f = _build.getEnv().get(new_path);

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

void Process::_lseek(int fd, off_t offset, int whence) {
  // TODO: track file descriptor offsets
  resume();
}
