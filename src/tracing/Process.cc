#include "Process.hh"

#include <memory>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>

#include "artifacts/Artifact.hh"
#include "build/Build.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "tracing/syscalls.hh"
#include "util/log.hh"
#include "util/path.hh"

using std::dynamic_pointer_cast;
using std::make_shared;

namespace fs = std::filesystem;

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

user_regs_struct Process::getRegisters() noexcept {
  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, _pid, nullptr, &regs)) << "Failed to get registers: " << ERR;
  return regs;
}

void Process::resume() noexcept {
  FAIL_IF(ptrace(PTRACE_CONT, _pid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

long Process::finishSyscall() noexcept {
  FAIL_IF(ptrace(PTRACE_SYSCALL, _pid, nullptr, 0)) << "Failed to finish syscall: " << ERR;
  FAIL_IF(waitpid(_pid, nullptr, 0) != _pid) << "Unexpected child process stop";

  // Clear errno so we can check for errors
  errno = 0;
  long result = ptrace(PTRACE_PEEKUSER, _pid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  return result;
}

unsigned long Process::getEventMessage() noexcept {
  // Get the id of the new process
  unsigned long message;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, _pid, nullptr, &message))
      << "Unable to read ptrace event message: " << ERR;
  return message;
}

shared_ptr<Access> Process::makeAccess(fs::path p, AccessFlags flags, int at) noexcept {
  // Absolute paths are resolved relative to the process' current root
  if (p.is_absolute()) return _command->access(p.relative_path(), flags, _root);

  // Handle the special CWD file descriptor to resolve relative to cwd
  if (at == AT_FDCWD) return _command->access(p.relative_path(), flags, _cwd);

  // The path is resolved relative to some file descriptor
  auto base = dynamic_pointer_cast<Access>(_fds.at(at).getReference());

  ASSERT(base) << "Attempted to resolve a path relative to an anonymous reference";

  return _command->access(p.relative_path(), flags, base);
}

string Process::readString(uintptr_t tracee_pointer) noexcept {
  // Strings are just char arrays terminated by '\0'
  auto data = readTerminatedArray<char, '\0'>(tracee_pointer);

  // Convert the result to a string
  return string(data.begin(), data.end());
}

// Read a value of type T from this process
template <typename T>
T Process::readData(uintptr_t tracee_pointer) noexcept {
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
vector<T> Process::readTerminatedArray(uintptr_t tracee_pointer) noexcept {
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

vector<string> Process::readArgvArray(uintptr_t tracee_pointer) noexcept {
  auto arg_pointers = readTerminatedArray<uintptr_t, 0>(tracee_pointer);

  vector<string> args;
  for (const auto& arg_ptr : arg_pointers) {
    args.push_back(readString(arg_ptr));
  }
  return args;
}

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

/************************* File Opening, Creation, and Closing ************************/

void Process::_openat(int dfd, string filename, int flags, mode_t mode) noexcept {
  // Get a reference from the given path
  auto ref = makeAccess(filename, AccessFlags::fromOpen(flags, mode), dfd);

  WARN_IF(ref->getFlags().directory)
      << "Accessing directory " << ref->getFullPath() << " with openat(). "
      << "This is not yet tracked correctly.";

  // Attempt to get an artifact using this reference *BEFORE* running the syscall.
  // This will ensure the environment knows whether or not this artifact is created
  ref->resolve(_command, _build);

  // Allow the syscall to finish, and record the result
  int fd = finishSyscall();

  // Let the process continue
  resume();

  // Check whether the openat call succeeded or failed
  if (fd >= 0) {
    // The command observed a successful openat, so add this predicate to the command log
    ref->expectResult(SUCCESS);

    ASSERT(ref->isResolved()) << "Failed to locate artifact for opened file: " << filename;

    // If the file is truncated by the open call, set the contents in the artifact
    if (ref->getFlags().truncate) {
      _command->setContents(ref);
    }

    // Is this new descriptor closed on exec?
    bool cloexec = ((flags & O_CLOEXEC) == O_CLOEXEC);

    // Record the reference in the correct location in this process' file descriptor table
    _fds.emplace(fd, FileDescriptor(ref, ref->getFlags().w, cloexec));

  } else {
    // The command observed a failed openat, so add the error predicate to the command log
    // Negate fd because syscalls return negative errors
    ref->expectResult(-fd);
  }
}

void Process::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) noexcept {
  WARN << "mknodat syscall is not updated";
  resume();
}

void Process::_close(int fd) noexcept {
  // NOTE: We assume close calls always succeed. Erasing a non-existent file descriptor is
  // harmless

  // Resume the process
  resume();

  // Remove the file descriptor
  _fds.erase(fd);
}

/************************ Pipes ************************/

void Process::_pipe2(int* fds, int flags) noexcept {
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
  ref->resolve(_command, _build);

  ASSERT(ref->isResolved()) << "Failed to get artifact for pipe";

  // The command sets the contents of the pipe on creation
  _command->setContents(ref);

  // Check if this pipe is closed on exec
  bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

  // Fill in the file descriptor entries
  _fds.emplace(read_pipefd, FileDescriptor(ref, false, cloexec));
  _fds.emplace(write_pipefd, FileDescriptor(ref, true, cloexec));
}

/************************ File Descriptor Manipulation ************************/

int Process::_dup(int fd) noexcept {
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

void Process::_dup3(int oldfd, int newfd, int flags) noexcept {
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

void Process::_fcntl(int fd, int cmd, unsigned long arg) noexcept {
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

void Process::_tee(int fd_in, int fd_out) noexcept {
  WARN << "tee syscall is not updated";
  resume();
}

/************************ Metadata Operations ************************/

void Process::_faccessat(int dirfd, string pathname, int mode, int flags) noexcept {
  // Create a reference
  auto ref = makeAccess(pathname, AccessFlags::fromAccess(mode, flags), dirfd);

  // Finish the syscall so we can see its result
  int rc = finishSyscall();

  // Resume the process' execution
  resume();

  // Record the outcome of the reference
  ref->expectResult(-rc);

  if (rc == 0) {
    ref->resolve(_command, _build);
    PREFER(ref->isResolved()) << "Failed to resolve reference " << ref;
    // Don't abort here becaues the dodo self-build accesses /proc/self.
    // We need to fix these references for real at some point.
  }
}

void Process::_fstatat(int dirfd, string pathname, int flags) noexcept {
  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if ((flags & AT_EMPTY_PATH) == AT_EMPTY_PATH) {
    resume();

    // This is essentially an fstat call
    // Record the dependency on metadata
    _command->metadataMatch(_fds.at(dirfd).getReference());

  } else {
    // This is a regular stat call (with an optional base directory descriptor)
    auto ref = makeAccess(pathname, {}, dirfd);

    // Finish the syscall to see if the reference succeeds
    int rc = finishSyscall();
    resume();

    // Record the outcome of the syscall
    ref->expectResult(-rc);

    // Log the success or failure
    if (rc == 0) {
      // Get the artifact that was stat-ed
      ref->resolve(_command, _build);

      ASSERT(ref->isResolved()) << "Unable to locate artifact for stat-ed file";

      // Record the dependence on the artifact's metadata
      _command->metadataMatch(ref);
    }
  }
}

void Process::_lchown(string filename, uid_t user, gid_t group) noexcept {
  WARN << "lchown syscall is not updated";
  resume();
}

void Process::_fchown(int fd, uid_t user, gid_t group) noexcept {
  WARN << "fchown syscall is not update";
  resume();
}

void Process::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flags) noexcept {
  WARN << "fchownat syscall is not updated";
  resume();
}

void Process::_fchmod(int fd, mode_t mode) noexcept {
  WARN << "fchmod syscall is not updated";
  resume();
}

void Process::_fchmodat(int dfd, string filename, mode_t mode, int flags) noexcept {
  // Make a reference to the file that will be chmod-ed.
  // TODO: We need permissions in the directory to chmod, right?
  auto ref = makeAccess(filename, AccessFlags{}, dfd);

  // Get the artifact that we're going to chmod
  ref->resolve(_command, _build);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->isResolved()) {
    _command->metadataMatch(ref);
  }

  // Finish the syscall and then resume the process
  int rc = finishSyscall();
  resume();

  // Did the call succeed?
  if (rc >= 0) {
    // Yes. Record the successful reference
    ref->expectResult(SUCCESS);

    ASSERT(ref->isResolved()) << "Failed to get artifact";

    // We've now set the artifact's metadata
    _command->setMetadata(ref);

  } else {
    // No. Record the failure
    ref->expectResult(-rc);
  }
}

/************************ File Content Operations ************************/

void Process::_read(int fd) noexcept {
  // Get the descriptor
  const auto& descriptor = _fds.at(fd);

  // The current command depends on the contents of this file
  _command->contentsMatch(descriptor.getReference());

  // We can't wait for the syscall to finish here because of this scenario:
  //  fd may be the read end of a pipe that is currently empty. The process that will write to the
  //  pipe is also blocked, but we're not handling it now. In that case, the syscall will not
  //  finish until we resume the *other* process. To handle this case correctly we'd need to place
  //  a wait for any child after resuming the blocked process. pre_ and post_ hooks for syscalls
  //  would work, but we don't always need them. Threads would also work, btu that creates other
  //  problems.
  resume();
}

void Process::_write(int fd) noexcept {
  // Get the descriptor
  const auto& descriptor = _fds.at(fd);

  // Record our dependency on the old contents of the artifact
  _command->contentsMatch(descriptor.getReference());

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  // If the write syscall failed, there's no need to log a write
  if (rc == -1) return;

  // Record the update to the artifact contents
  _command->setContents(descriptor.getReference());
}

void Process::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) noexcept {
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
  const auto& descriptor = _fds.at(fd);

  // By mmapping a file, the command implicitly depends on its contents at the time of
  // mapping.
  _command->contentsMatch(descriptor.getReference());

  // If the mapping is writable, and the file was opened in write mode, the command
  // is also effectively setting the contents of the file.
  bool writable = (prot & PROT_WRITE) && descriptor.isWritable();
  if (writable) {
    _command->setContents(descriptor.getReference());
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

void Process::_sendfile(int out_fd, int in_fd) noexcept {
  WARN << "sendfile syscall is not updated";
  resume();
}

void Process::_truncate(string pathname, long length) noexcept {
  auto ref = makeAccess(pathname, AccessFlags{.w = true});

  // Get the artifact that's being truncated
  ref->resolve(_command, _build);

  // If length is non-zero, we depend on the previous contents
  // This only applies if the artifact exists
  if (length > 0 && ref->isResolved()) {
    _command->contentsMatch(ref);
  }

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  // Record the outcome of the reference
  ref->expectResult(-rc);

  // Did the call succeed?
  if (rc == 0) {
    // Make sure the artifact actually existed
    ASSERT(ref->isResolved()) << "Failed to get artifact for truncated file";

    // Record the update to the artifact contents
    _command->setContents(ref);
  }
}

void Process::_ftruncate(int fd, long length) noexcept {
  // Get the descriptor
  const auto& descriptor = _fds.at(fd);

  // If length is non-zero, this is a write so we depend on the previous contents
  if (length > 0) {
    _command->contentsMatch(descriptor.getReference());
  }

  // Finish the syscall and resume the process
  int rc = finishSyscall();
  resume();

  if (rc == 0) {
    // Record the update to the artifact contents
    _command->setContents(descriptor.getReference());
  }
}

void Process::_vmsplice(int fd) noexcept {
  WARN << "vmsplice syscall is not updated";
  resume();
}

/************************ Directory Operations ************************/

void Process::_mkdirat(int dfd, string pathname, mode_t mode) noexcept {
  WARN << "mkdirat syscall is not updated";
  resume();
}

void Process::_rmdir(string p) noexcept {
  WARN << "rmdir syscall is not updated";
  resume();
}

void Process::_renameat2(int old_dfd, string oldpath, int new_dfd, string newpath,
                         int flags) noexcept {
  WARN << "renameat2 syscall is not updated";
  resume();
}

void Process::_getdents(int fd) noexcept {
  WARN << "getdents syscall is not updated";
  resume();
}

/************************ Link and Symlink Operations ************************/

void Process::_symlinkat(string oldname, int newdfd, string newname) noexcept {
  WARN << "symlinkat syscall is not updated";
  resume();
}

void Process::_readlinkat(int dfd, string pathname) noexcept {
  // We need a better way to blacklist /proc/self tracking, but this is enough to make the self
  // build work
  if (pathname.find("/proc/self") != string::npos) {
    resume();
    return;
  }

  // We're making a reference to a symlink, so don't follow links
  auto ref = makeAccess(pathname, AccessFlags{.nofollow = true}, dfd);

  // Finish the syscall and then resume the process
  int rc = finishSyscall();
  resume();

  // Did the call succeed?
  if (rc >= 0) {
    // Yes. Record the successful reference
    ref->expectResult(SUCCESS);

    // Get the artifact that we referenced
    ref->resolve(_command, _build);

    ASSERT(ref->isResolved()) << "Failed to get artifact for successfully-read link";

    // We depend on this artifact's contents now
    _command->contentsMatch(ref);

  } else {
    // No. Record the failure
    ref->expectResult(-rc);
  }
}

void Process::_unlinkat(int dfd, string pathname, int flags) noexcept {
  WARN << "unlinkat syscall is not updated";
  resume();
}

/************************ Process State Operations ************************/

void Process::_chdir(string filename) noexcept {
  int rc = finishSyscall();
  resume();

  // Update the current working directory if the chdir call succeeded
  if (rc == 0) {
    _cwd = makeAccess(filename, AccessFlags{.x = true});
    _cwd->expectResult(SUCCESS);
    _cwd->resolve(_command, _build);
    ASSERT(_cwd->isResolved()) << "Failed to resolve current working directory";
  }
}

void Process::_chroot(string filename) noexcept {
  WARN << "chroot syscall is not updated";
  resume();
}

void Process::_pivot_root(string new_root, string put_old) noexcept {
  WARN << "pivot_root syscall is not updated";
  resume();
}

void Process::_fchdir(int fd) noexcept {
  int rc = finishSyscall();
  resume();

  if (rc == 0) {
    // Get the path to the artifact this descriptor references
    const auto& descriptor = _fds.at(fd);
    auto a = dynamic_pointer_cast<Access>(descriptor.getReference());

    // Make sure there really is a path
    ASSERT(a) << "fchdir to an artifact with no path should not succeed";

    // Update the working directory
    _cwd = a;
  }
}

void Process::_execveat(int dfd, string filename, vector<string> args,
                        vector<string> env) noexcept {
  // The parent command needs execute access to the exec-ed path
  auto exe_ref = makeAccess(filename, AccessFlags{.x = true}, dfd);

  // Finish the exec syscall and resume
  int rc = finishSyscall();
  resume();

  // Not sure why, but exec returns -38 on success.
  // If we see something else, handle the error
  if (rc != -38) {
    // Failure! Record a failed reference. Negate rc because syscalls return negative errors
    exe_ref->expectResult(-rc);
    return;
  }

  // If we reached this point, the executable reference was okay
  exe_ref->expectResult(SUCCESS);

  // Resolve the reference to the executable file
  exe_ref->resolve(_command, _build);
  ASSERT(exe_ref->isResolved()) << "Executable file failed to resolve";

  // Build a map of the initial file descriptors for the child command
  // As we build this map, keep track of which file descriptors have to be erased from the
  // process' current map of file descriptors.
  map<int, FileDescriptor> initial_fds;
  list<int> to_erase;

  for (const auto& [index, fd] : _fds) {
    if (fd.isCloexec()) {
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, FileDescriptor(fd.getReference(), fd.isWritable()));
    }
  }
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // This process launches a new command
  _command = _command->launch(exe_ref, args, initial_fds, _cwd, _root);

  // The child command depends on the contents of its executable. First, we need to know what the
  // actual executable is. Read /proc/<pid>/exe to find it
  auto real_exe_path = readlink("/proc/" + std::to_string(_pid) + "/exe");

  // Now make the reference and expect success
  auto child_exe_ref = makeAccess(real_exe_path, AccessFlags{.r = true});
  child_exe_ref->expectResult(SUCCESS);

  // Resolve the child executable reference
  child_exe_ref->resolve(_command, _build);
  ASSERT(child_exe_ref->isResolved()) << "Failed to locate artifact for executable file";

  // The child command depends on the contents of the executable
  _command->contentsMatch(child_exe_ref);

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
  // that participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}
