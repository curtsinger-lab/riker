#include "Process.hh"

#include <functional>
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
#include "tracing/SyscallTable.hh"
#include "tracing/Tracer.hh"
#include "util/log.hh"
#include "util/path.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"

using std::function;

namespace fs = std::filesystem;

/*******************************************/
/********** Utilities for tracing **********/
/*******************************************/

// Update a process' working directory
void Process::setWorkingDir(shared_ptr<Access> ref) noexcept {
  _cwd->expectResult(SUCCESS);
  ASSERT(_cwd->isResolved()) << "Failed to resolve current working directory";
  _cwd = ref;
}

// Get a file descriptor entry
FileDescriptor& Process::getFD(int fd) noexcept {
  ASSERT(_fds.find(fd) != _fds.end())
      << "Attempted to access an unknown fd " << fd << " in " << this;
  return _fds.at(fd);
}

// Add a file descriptor entry
FileDescriptor& Process::addFD(int fd, shared_ptr<Ref> ref, bool writable, bool cloexec) noexcept {
  auto [iter, added] = _fds.emplace(fd, FileDescriptor(ref, writable, cloexec));
  ASSERT(added) << "Attempted to overwrite an existing fd " << fd << " in " << this;
  return iter->second;
}

// Close a file descriptor
void Process::closeFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    WARN << "Closing an unknown file descriptor " << fd << " in " << this;
  } else {
    _fds.erase(iter);
  }
}

// Remove a file descriptor entry if it exists
void Process::tryCloseFD(int fd) noexcept {
  _fds.erase(fd);
}

// The process is executing a new file
void Process::exec(shared_ptr<Access> exe_ref, vector<string> args, vector<string> env) noexcept {
  // Build a map of the initial file descriptors for the child command
  // As we build this map, keep track of which file descriptors have to be erased from the
  // process' current map of file descriptors.
  map<int, FileDescriptor> initial_fds;
  list<int> to_erase;

  for (const auto& [index, fd] : _fds) {
    if (fd.isCloexec()) {
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, FileDescriptor(fd.getRef(), fd.isWritable()));
    }
  }
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // Create the child command
  auto child = make_shared<Command>(exe_ref, args, initial_fds, _cwd, _root);

  // Inform the build of the launch action
  _build.launch(_command, child);

  // This process is now running the child
  _command = child;

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
  // that participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}

shared_ptr<Access> Thread::makeAccess(fs::path p, AccessFlags flags, int at) noexcept {
  // Absolute paths are resolved relative to the process' current root
  if (p.is_absolute())
    return _build.access(_process->getCommand(), _process->getRoot(), p.relative_path(), flags);

  // Handle the special CWD file descriptor to resolve relative to cwd
  if (at == AT_FDCWD)
    return _build.access(_process->getCommand(), _process->getWorkingDir(), p.relative_path(),
                         flags);

  // The path is resolved relative to some file descriptor
  auto descriptor = _process->getFD(at);
  auto base = descriptor.getRef()->as<Access>();

  ASSERT(base) << "Attempted to resolve path " << p << " relative to anonymous base "
               << descriptor.getRef();

  return _build.access(_process->getCommand(), base, p.relative_path(), flags);
}

user_regs_struct Thread::getRegisters() noexcept {
  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, _tid, nullptr, &regs))
      << "Failed to get registers (for PID " << _tid << "): " << ERR;
  return regs;
}

void Thread::setRegisters(user_regs_struct& regs) noexcept {
  FAIL_IF(ptrace(PTRACE_SETREGS, _tid, nullptr, &regs)) << "Failed to set registers: " << ERR;
}

void Thread::resume() noexcept {
  FAIL_IF(ptrace(PTRACE_CONT, _tid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

void Thread::finishSyscall(function<void(long)> handler) noexcept {
  ASSERT(!_post_syscall_handler) << "Process already has an unexecuted post-syscall handler";

  // Set the post-syscall handler
  _post_syscall_handler = handler;

  // Allow the tracee to resume until its syscall finishes
  FAIL_IF(ptrace(PTRACE_SYSCALL, _tid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

void Thread::syscallFinished() noexcept {
  ASSERT(_post_syscall_handler) << "Process does not have a post-syscall handler";

  // Set up an empty handler
  function<void(long)> handler;

  // Swap it with the registered handler (to clear the registered one)
  _post_syscall_handler.swap(handler);

  // Now extract the return code from the syscall

  // Clear errno so we can check for errors
  errno = 0;
  long result = ptrace(PTRACE_PEEKUSER, _tid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  // Run the handler
  handler(result);
}

unsigned long Thread::getEventMessage() noexcept {
  // Get the id of the new process
  unsigned long message;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, _tid, nullptr, &message))
      << "Unable to read ptrace event message: " << ERR;
  return message;
}

string Thread::readString(uintptr_t tracee_pointer) noexcept {
  // Strings are just char arrays terminated by '\0'
  auto data = readTerminatedArray<char, '\0'>(tracee_pointer);

  // Convert the result to a string
  return string(data.begin(), data.end());
}

// Read a value of type T from this process
template <typename T>
T Thread::readData(uintptr_t tracee_pointer) noexcept {
  // Reserve space for the value we will read
  T result{};

  // If the tracee pointer is null, return the defult value
  if (tracee_pointer == 0) return result;

  // Set up iovec structs for the remote read and local write
  struct iovec local = {.iov_base = &result, .iov_len = sizeof(T)};
  struct iovec remote = {.iov_base = (void*)tracee_pointer, .iov_len = sizeof(T)};

  // Do the read
  auto rc = process_vm_readv(_tid, &local, 1, &remote, 1, 0);
  // Check the result
  FAIL_IF(rc != sizeof(T)) << this << ": Error in readData(" << (void*)tracee_pointer << "). "
                           << ERR;

  return result;
}

// Read an array of values up to a terminating value
template <typename T, T Terminator, size_t BatchSize>
vector<T> Thread::readTerminatedArray(uintptr_t tracee_pointer) noexcept {
  // If the pointer is null, return an empty array
  if (tracee_pointer == 0) return vector<T>();

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
    auto rc = process_vm_readv(_tid, &local, 1, &remote, 1, 0);

    // Check for failure
    FAIL_IF(rc == -1) << this << ": Error in readTerminatedArray(" << (void*)tracee_pointer << "). "
                      << ERR;

    // The return code is the number of bytes read. This will often be BatchSize * sizeof(T), but
    // can be smaller. Advance position (the index into the output array) by the number of complete
    // elements read.
    position += rc / sizeof(T);

    // Let the result vector know we're about to append a bunch of data
    result.reserve(result.size() + rc / sizeof(T));

    // Scan for a terminator
    for (size_t i = 0; i < rc / sizeof(T); i++) {
      // If we find a terminator, it's time to return
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

vector<string> Thread::readArgvArray(uintptr_t tracee_pointer) noexcept {
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

void Thread::_openat(int dfd, string filename, int flags, mode_t mode) noexcept {
  LOGF(trace, "{}: openat({}, \"{}\", {}, {:o})", this, dfd, filename, flags, mode);

  // Get a reference from the given path
  // Attempt to get an artifact using this reference *BEFORE* running the syscall.
  // This will ensure the environment knows whether or not this artifact is created
  auto ref = makeAccess(filename, AccessFlags::fromOpen(flags, mode), dfd);

  // Allow the syscall to finish
  finishSyscall([=](long fd) {
    // Let the process continue
    resume();

    // Check whether the openat call succeeded or failed
    if (fd >= 0) {
      // The command observed a successful openat, so add this predicate to the command log
      ref->expectResult(SUCCESS);

      ASSERT(ref->isResolved()) << "Failed to locate artifact for opened file: " << filename;

      // Is this new descriptor closed on exec?
      bool cloexec = ((flags & O_CLOEXEC) == O_CLOEXEC);

      // If the O_TMPFILE flag was passed, this call created a reference to an anonymous file
      if ((flags & O_TMPFILE) == O_TMPFILE) {
        auto anon = _build.file(_process->getCommand(), mode);

        // Record the reference in the process' file descriptor table
        _process->addFD(fd, anon, ref->getFlags().w, cloexec);

      } else {
        // If the file is truncated by the open call, set the contents in the artifact
        if (ref->getFlags().truncate) {
          auto written = make_shared<FileVersion>(FileFingerprint::makeEmpty());
          _build.updateContent(_process->getCommand(), ref, written);
        }

        // Record the reference in the correct location in this process' file descriptor table
        _process->addFD(fd, ref, ref->getFlags().w, cloexec);
      }

    } else {
      // The command observed a failed openat, so add the error predicate to the command log
      // Negate fd because syscalls return negative errors
      ref->expectResult(-fd);
    }
  });
}

void Thread::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) noexcept {
  LOGF(trace, "{}: mknodat({}, \"{}\", {:o}, {})", this, dfd, filename, mode, dev);

  if ((mode & S_IFMT) == S_IFREG) {
    // Handle regular file creation with openat
    _openat(dfd, filename, O_CREAT | O_EXCL, mode);
  } else {
    // TODO: Handle named pipes?

    WARN << "Unsupported use of mknodat";
    resume();
  }
}

void Thread::_close(int fd) noexcept {
  LOGF(trace, "{}: close({})", this, fd);

  finishSyscall([=](long rc) {
    // Resume the blocked thread
    resume();

    // If the syscall succeeded, remove the file descriptor
    if (rc == 0) _process->closeFD(fd);
  });
}

/************************ Pipes ************************/

void Thread::_pipe2(int* fds, int flags) noexcept {
  LOGF(trace, "{}: pipe2({}, {})", this, (void*)fds, flags);

  finishSyscall([=](long rc) {
    // There is nothing to do if the syscall fails, but why would that ever happen?
    if (rc) {
      resume();
      return;
    }

    // Read the file descriptors
    int read_pipefd = readData((uintptr_t)fds);
    int write_pipefd = readData((uintptr_t)fds + sizeof(int));

    // The command can continue
    resume();

    // Make a reference to a pipe
    auto ref = _build.pipe(_process->getCommand());

    ASSERT(ref->isResolved()) << "Failed to get artifact for pipe";

    // Check if this pipe is closed on exec
    bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

    // Fill in the file descriptor entries
    _process->addFD(read_pipefd, ref, false, cloexec);
    _process->addFD(write_pipefd, ref, true, cloexec);
  });
}

/************************ File Descriptor Manipulation ************************/

void Thread::_dup(int fd) noexcept {
  LOGF(trace, "{}: dup({})", this, fd);

  // Is the provided file descriptor valid?
  if (_process->hasFD(fd)) {
    // Finish the syscall to get the new file descriptor, then resume the process
    finishSyscall([=](int newfd) {
      resume();

      // If the syscall failed, do nothing
      if (newfd == -1) return;

      // Add the new entry for the duped fd. The cloexec flag is not inherited, so it's always
      // false.
      auto& descriptor = _process->getFD(fd);
      _process->addFD(newfd, descriptor.getRef(), descriptor.isWritable(), false);
    });
  } else {
    finishSyscall([=](long rc) {
      resume();
      ASSERT(rc == -EBADF) << "dup of invalid file descriptor did not fail with EBADF";
    });
  }
}

void Thread::_dup3(int oldfd, int newfd, int flags) noexcept {
  LOGF(trace, "{}: dup3({}, {}, {})", this, oldfd, newfd, flags);

  // dup3 returns the new file descriptor, or error
  // Finish the syscall so we know what file descriptor to add to our table
  if (_process->hasFD(oldfd)) {
    finishSyscall([=](long rc) {
      resume();

      // If the syscall failed, we have nothing more to do
      // Note: this is different than a failed file access. This failure should not be affected
      //       by the state of the filesystem, so we don't have to log it.
      if (rc == -1) return;

      // If there is an existing descriptor entry number newfd, it is silently closed
      _process->tryCloseFD(newfd);

      // The new descriptor is only marked cloexec if the flag is provided.
      bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

      // Duplicate the file descriptor
      auto& descriptor = _process->getFD(oldfd);
      _process->addFD(rc, descriptor.getRef(), descriptor.isWritable(), cloexec);
    });
  } else {
    finishSyscall([=](long rc) {
      resume();
      ASSERT(rc == -EBADF) << "dup3 of invalid file descriptor did not fail with EBADF";
    });
  }
}

void Thread::_fcntl(int fd, int cmd, unsigned long arg) noexcept {
  LOGF(trace, "{}: fcntl({}, {}, {})", this, fd, cmd, arg);

  if (cmd == F_DUPFD) {
    // Handle fcntl(F_DUPFD) as a dup call. The return value is the new fd.
    _dup(fd);  // _dup will resume the process and return the new fd to us

  } else if (cmd == F_DUPFD_CLOEXEC) {
    // fcntl(F_DUPFD_CLOEXEC) is just like a dup call, followed by setting cloexec to true
    // int newfd = _dup(fd);  // _dup will resume the process and return the new fd to us
    // _fds.at(newfd).setCloexec(true);
    _dup3(fd, -1, FD_CLOEXEC);

  } else if (cmd == F_SETFD) {
    resume();
    // Set the cloexec flag using the argument flags
    _process->getFD(fd).setCloexec(arg & FD_CLOEXEC);

  } else {
    resume();
    // Some other operation we do not need to handle
    // TODO: Filter these stops out with BPF/seccomp
  }
}

/************************ Metadata Operations ************************/

void Thread::_faccessat(int dirfd, string pathname, int mode, int flags) noexcept {
  LOGF(trace, "{}: faccessat({}, \"{}\", {:o}, {})", this, dirfd, pathname, mode, flags);

  // Finish the syscall so we can see its result
  finishSyscall([=](long rc) {
    // Resume the process' execution
    resume();

    // Create a reference
    auto ref = makeAccess(pathname, AccessFlags::fromAccess(mode, flags), dirfd);

    // Record the outcome of the reference
    ref->expectResult(-rc);

    if (rc == 0) {
      if (!ref->isResolved()) WARN << "Failed to resolve reference " << ref;
      // Don't abort here because the dodo self-build accesses /proc/self.
      // We need to fix these references for real at some point.
    }
  });
}

void Thread::_fstatat(int dirfd, string pathname, struct stat* statbuf, int flags) noexcept {
  LOGF(trace, "{}: fstatat({}, \"{}\", {}, {})", this, dirfd, pathname, (void*)statbuf, flags);

  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if ((flags & AT_EMPTY_PATH) == AT_EMPTY_PATH) {
    resume();

    // This is essentially an fstat call
    // Record the dependency on metadata
    _build.matchMetadata(_process->getCommand(), _process->getFD(dirfd).getRef());

  } else {
    // Finish the syscall to see if the reference succeeds
    finishSyscall([=](long rc) {
      resume();

      if (rc == 0) {
        // The stat succeeded
        auto ref = makeAccess(pathname, {}, dirfd);
        ref->expectResult(SUCCESS);
        ASSERT(ref->isResolved()) << "Unable to locate artifact for stat-ed file " << ref;

        // Record the dependence on the artifact's metadata
        _build.matchMetadata(_process->getCommand(), ref);

      } else if (rc == -EACCES || rc == -ENOENT || rc == -ENOTDIR) {
        // The stat failed with a filesystem-related error
        auto ref = makeAccess(pathname, {}, dirfd);
        ref->expectResult(-rc);
      } else {
        // The stat failed with some other error that doesn't matter to us. We see this in rustc.
      }
    });
  }
}

void Thread::_fchown(int fd, uid_t user, gid_t group) noexcept {
  LOGF(trace, "{}: fchown({}, {}, {})", this, fd, user, group);

  // Get the file descriptor
  auto& descriptor = _process->getFD(fd);

  // The command depends on the old metadata
  _build.matchMetadata(_process->getCommand(), descriptor.getRef());

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.updateMetadata(_process->getCommand(), descriptor.getRef());
  });
}

void Thread::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flags) noexcept {
  LOGF(trace, "{}: fchownat({}, \"{}\", {}, {}, {})", this, dfd, filename, user, group, flags);

  // Make a reference to the file that will be chown-ed.
  bool nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
  auto ref = makeAccess(filename, AccessFlags{.nofollow = nofollow}, dfd);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->isResolved()) {
    _build.matchMetadata(_process->getCommand(), ref);
  }

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      ref->expectResult(SUCCESS);

      ASSERT(ref->isResolved()) << "Failed to get artifact";

      // We've now set the artifact's metadata
      _build.updateMetadata(_process->getCommand(), ref);

    } else {
      // No. Record the failure
      ref->expectResult(-rc);
    }
  });
}

void Thread::_fchmod(int fd, mode_t mode) noexcept {
  LOGF(trace, "{}: fchmod({}, {:o})", this, fd, mode);

  // Get the file descriptor entry
  auto& descriptor = _process->getFD(fd);

  // The command depends on the old metadata
  _build.matchMetadata(_process->getCommand(), descriptor.getRef());

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.updateMetadata(_process->getCommand(), descriptor.getRef());
  });
}

void Thread::_fchmodat(int dfd, string filename, mode_t mode, int flags) noexcept {
  LOGF(trace, "{}: fchmodat({}, \"{}\", {:o}, {})", this, dfd, filename, mode, flags);

  // Make a reference to the file that will be chmod-ed.
  bool nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
  auto ref = makeAccess(filename, AccessFlags{.nofollow = nofollow}, dfd);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->isResolved()) {
    _build.matchMetadata(_process->getCommand(), ref);
  }

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      ref->expectResult(SUCCESS);

      ASSERT(ref->isResolved()) << "Failed to get artifact";

      // We've now set the artifact's metadata
      _build.updateMetadata(_process->getCommand(), ref);

    } else {
      // No. Record the failure
      ref->expectResult(-rc);
    }
  });
}

/************************ File Content Operations ************************/

void Thread::_read(int fd) noexcept {
  LOGF(trace, "{}: read({})", this, fd);

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // Create a dependency on the artifact's contents
    const auto& descriptor = _process->getFD(fd);
    _build.matchContent(_process->getCommand(), descriptor.getRef());
  });
}

void Thread::_write(int fd) noexcept {
  LOGF(trace, "{}: write({})", this, fd);

  // Get the descriptor
  const auto& descriptor = _process->getFD(fd);

  // Record our dependency on the old contents of the artifact
  _build.matchContent(_process->getCommand(), descriptor.getRef());

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the write syscall failed, there's no need to log a write
    if (rc == -1) return;

    // Record the update to the artifact contents
    _build.updateContent(_process->getCommand(), descriptor.getRef());
  });
}

void Thread::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) noexcept {
  LOGF(trace, "{}: mmap({})", this, fd);

  // Skip anonymous mappings. We never need to handle these because they only allow communication
  // within a single command.
  if (fd == -1) {
    LOGF(trace, "{}: skipped anonymous mmap({})", this, fd);
    resume();
    return;
  }

  // Run the syscall to find out if the mmap succeeded
  finishSyscall([=](long rc) {
    LOGF(trace, "{}: finished mmap({})", this, fd);
    void* result = (void*)rc;

    // If the map failed there's nothing to log
    if (result == MAP_FAILED) {
      resume();
      return;
    }

    // Get the descriptor from the fd number
    const auto& descriptor = _process->getFD(fd);

    // By mmapping a file, the command implicitly depends on its contents at the time of
    // mapping.
    _build.matchContent(_process->getCommand(), descriptor.getRef());

    // If the mapping is writable, and the file was opened in write mode, the command
    // is also effectively setting the contents of the file.
    bool writable = (prot & PROT_WRITE) && descriptor.isWritable();
    if (writable) {
      _build.updateContent(_process->getCommand(), descriptor.getRef());
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
  });
}

void Thread::_truncate(string pathname, long length) noexcept {
  LOGF(trace, "{}: truncate(\"{}\", {})", this, pathname, length);

  // Make an access to the reference that will be truncated
  auto ref = makeAccess(pathname, AccessFlags{.w = true});

  // If length is non-zero, we depend on the previous contents
  // This only applies if the artifact exists
  if (length > 0 && ref->isResolved()) {
    _build.matchContent(_process->getCommand(), ref);
  }

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // Record the outcome of the reference
    ref->expectResult(-rc);

    // Did the call succeed?
    if (rc == 0) {
      // Make sure the artifact actually existed
      ASSERT(ref->isResolved()) << "Failed to get artifact for truncated file";

      // Record the update to the artifact contents
      _build.updateContent(_process->getCommand(), ref);
    }
  });
}

void Thread::_ftruncate(int fd, long length) noexcept {
  LOGF(trace, "{}: ftruncate({}, {})", this, fd, length);

  // Get the descriptor
  const auto& descriptor = _process->getFD(fd);

  // If length is non-zero, this is a write so we depend on the previous contents
  if (length > 0) {
    _build.matchContent(_process->getCommand(), descriptor.getRef());
  }

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Record the update to the artifact contents
      _build.updateContent(_process->getCommand(), descriptor.getRef());
    }
  });
}

void Thread::_tee(int fd_in, int fd_out) noexcept {
  LOGF(trace, "{}: tee({}, {})", this, fd_in, fd_out);

  // Get the descriptors
  const auto& in_desc = _process->getFD(fd_in);
  const auto& out_desc = _process->getFD(fd_out);

  // The command depends on the contents of the output file, unless it is totally overwritten (not
  // checked yet)
  _build.matchContent(_process->getCommand(), out_desc.getRef());

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // The command has now read the input file, so it depends on the contents there
    _build.matchContent(_process->getCommand(), in_desc.getRef());

    // The command has now set the contents of the output file
    _build.updateContent(_process->getCommand(), out_desc.getRef());
  });
}

/************************ Directory Operations ************************/

void Thread::_mkdirat(int dfd, string pathname, mode_t mode) noexcept {
  LOGF(trace, "{}: mkdirat({}, \"{}\", {:o})", this, dfd, pathname, mode);

  auto full_path = fs::path(pathname);
  auto parent_path = full_path.parent_path();
  auto entry = full_path.filename();

  // Make a reference to the parent directory where the new directory will be added
  auto parent_ref = makeAccess(parent_path, AccessFlags{.w = true}, dfd);

  // Make a reference to the new directory entry that will be created
  auto entry_ref = makeAccess(full_path, AccessFlags{}, dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Write access to the parent directory must succeed
      parent_ref->expectResult(SUCCESS);

      // The entry must not exist prior to this call
      entry_ref->expectResult(ENOENT);

      // Make a directory reference to get a new artifact
      auto dir_ref = _build.dir(_process->getCommand(), mode);

      // Link the directory into the parent dir
      _build.updateContent(_process->getCommand(), parent_ref,
                           make_shared<AddEntry>(entry, dir_ref));

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      parent_ref->expectResult(parent_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
    }
  });
}

void Thread::_renameat2(int old_dfd,
                        string old_name,
                        int new_dfd,
                        string new_name,
                        int flags) noexcept {
  LOGF(trace, "{}: renameat({}, \"{}\", {}, \"{}\", {})", this, old_dfd, old_name, new_dfd,
       new_name, flags);

  // Break the path to the existing file into directory and entry parts
  auto old_path = fs::path(old_name);
  auto old_dir = old_path.parent_path();
  auto old_entry = old_path.filename();

  // Make references to the old directory and entry
  auto old_dir_ref = makeAccess(old_dir, AccessFlags{.w = true}, old_dfd);
  auto old_entry_ref = makeAccess(old_path, AccessFlags{.nofollow = true}, old_dfd);

  // Break the path to the new file into directory and entry parts
  auto new_path = fs::path(new_name);
  auto new_dir = new_path.parent_path();
  auto new_entry = new_path.filename();

  // Make a reference to the new directory
  auto new_dir_ref = makeAccess(new_dir, AccessFlags{.w = true}, new_dfd);

  // If either RENAME_EXCHANGE or RENAME_NOREPLACE is specified, make a reference to the new entry
  shared_ptr<Access> new_entry_ref;
  if ((flags & RENAME_EXCHANGE) || (flags & RENAME_NOREPLACE)) {
    new_entry_ref = makeAccess(new_path, AccessFlags{.nofollow = true}, new_dfd);
  }

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // The accesses to the old directory and entry must have succeeded
      old_dir_ref->expectResult(SUCCESS);
      old_entry_ref->expectResult(SUCCESS);

      // Unlink the old entry
      _build.updateContent(_process->getCommand(), old_dir_ref,
                           make_shared<RemoveEntry>(old_entry, old_entry_ref));

      // The access to the new directory must also have succeeded
      new_dir_ref->expectResult(SUCCESS);

      // Is this an exchange or noreplace option?
      if (flags & RENAME_EXCHANGE) {
        // This is an exchange, so the new_entry_ref must exist
        new_entry_ref->expectResult(SUCCESS);

        // Unlink the new entry
        _build.updateContent(_process->getCommand(), new_dir_ref,
                             make_shared<RemoveEntry>(new_entry, new_entry_ref));

      } else if (flags & RENAME_NOREPLACE) {
        // This is a noreplace rename, so new_entry_ref must not exist
        new_entry_ref->expectResult(ENOENT);
      }

      // Link into the new entry
      _build.updateContent(_process->getCommand(), new_dir_ref,
                           make_shared<AddEntry>(new_entry, old_entry_ref));

      // If this is an exchange, we also have to perform the swapped link
      if (flags & RENAME_EXCHANGE) {
        _build.updateContent(_process->getCommand(), old_dir_ref,
                             make_shared<AddEntry>(old_entry, new_entry_ref));
      }
    } else {
      // The syscall failed. Be conservative and save the result of all references. If any of them
      // change, that COULD change the syscall outcome.
      old_dir_ref->expectResult(old_dir_ref->getResolution());
      old_entry_ref->expectResult(old_entry_ref->getResolution());
      new_dir_ref->expectResult(new_dir_ref->getResolution());
      if (new_entry_ref) new_entry_ref->expectResult(new_entry_ref->getResolution());
    }
  });
}

void Thread::_getdents(int fd) noexcept {
  LOGF(trace, "{}: getdents({})", this, fd);

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    if (rc != -1) {
      // Create a dependency on the artifact's directory list
      const auto& descriptor = _process->getFD(fd);
      _build.matchContent(_process->getCommand(), descriptor.getRef());
    }
  });
}

/************************ Link and Symlink Operations ************************/

void Thread::_linkat(int old_dfd, string oldpath, int new_dfd, string newpath, int flags) noexcept {
  LOGF(trace, "{}: linkat({}, \"{}\", {}, \"{}\", {})", this, old_dfd, oldpath, new_dfd, newpath,
       flags);

  // The newpath string is the path to the new link. Split that into the directory and entry.
  auto link_path = fs::path(newpath);
  auto dir_path = link_path.parent_path();
  auto entry = link_path.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makeAccess(dir_path, AccessFlags{.w = true}, new_dfd);

  // Get a reference to the link we are creating
  auto entry_ref = makeAccess(link_path, AccessFlags{}, new_dfd);

  // Get a reference to the artifact we are linking into the directory
  AccessFlags target_flags = {.nofollow = true};
  if (flags & AT_SYMLINK_FOLLOW) target_flags.nofollow = false;
  auto target_ref = makeAccess(oldpath, target_flags, old_dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc == 0) {
      // Write access to the directory must succeed
      dir_ref->expectResult(SUCCESS);

      // The link must not exist prior to this call
      entry_ref->expectResult(ENOENT);

      // The reference to the link target must succeed
      target_ref->expectResult(SUCCESS);

      // Record the link operation
      _build.updateContent(_process->getCommand(), dir_ref,
                           make_shared<AddEntry>(entry, target_ref));

    } else {
      // The failure could be caused by the dir_ref, entry_ref, or target_ref. To be safe, just
      // record the result of resolving each of them.
      dir_ref->expectResult(dir_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
      target_ref->expectResult(target_ref->getResolution());
    }
  });
}

void Thread::_symlinkat(string target, int dfd, string newpath) noexcept {
  LOGF(trace, "{}: symlinkat(\"{}\", {}, \"{}\")", this, target, dfd, newpath);

  // The newpath string is the path to the new link. Split that into the directory and entry.
  auto link_path = fs::path(newpath);
  auto dir_path = link_path.parent_path();
  auto entry = link_path.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makeAccess(dir_path, AccessFlags{.w = true}, dfd);

  // Get a reference to the link we are creating
  auto entry_ref = makeAccess(link_path, AccessFlags{}, dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Write access to the directory must succeed
      dir_ref->expectResult(SUCCESS);

      // The link must not exist prior to this call
      entry_ref->expectResult(ENOENT);

      // Make a symlink reference to get a new artifact
      auto symlink_ref = _build.symlink(_process->getCommand(), target);

      // Link the symlink into the directory
      _build.updateContent(_process->getCommand(), dir_ref,
                           make_shared<AddEntry>(entry, symlink_ref));

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      dir_ref->expectResult(dir_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
    }
  });
}

void Thread::_readlinkat(int dfd, string pathname) noexcept {
  LOGF(trace, "{}: readlinkat({}, \"{}\")", this, dfd, pathname);

  // We need a better way to blacklist /proc/self tracking, but this is enough to make the self
  // build work
  if (pathname.find("/proc/self") != string::npos) {
    resume();
    return;
  }

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // We're making a reference to a symlink, so don't follow links
    auto ref = makeAccess(pathname, AccessFlags{.nofollow = true}, dfd);

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      ref->expectResult(SUCCESS);

      ASSERT(ref->isResolved()) << "Failed to get artifact for successfully-read link";

      // We depend on this artifact's contents now
      _build.matchContent(_process->getCommand(), ref);

    } else {
      // No. Record the failure
      ref->expectResult(-rc);
    }
  });
}

void Thread::_unlinkat(int dfd, string pathname, int flags) noexcept {
  LOGF(trace, "{}: unlinkat({}, \"{}\", {})", this, dfd, pathname, flags);

  // TODO: Make sure pathname does not refer to a directory, unless AT_REMOVEDIR is set

  // Split the pathname into the parent and entry
  auto path = fs::path(pathname);
  auto dir_path = path.parent_path();
  auto entry = path.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makeAccess(dir_path, AccessFlags{.w = true}, dfd);

  // Get a reference to the entry itself
  auto entry_ref = makeAccess(path, AccessFlags{.nofollow = true}, dfd);

  // If this call is removing a directory, depend on the directory contents
  if (entry_ref->isResolved()) {
    if (auto dir = entry_ref->getArtifact()->as<DirArtifact>()) {
      _build.matchContent(_process->getCommand(), entry_ref);
    }
  }

  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc == 0) {
      // Both references must have succeeded
      dir_ref->expectResult(SUCCESS);
      entry_ref->expectResult(SUCCESS);

      // Perform the unlink
      _build.updateContent(_process->getCommand(), dir_ref,
                           make_shared<RemoveEntry>(entry, entry_ref));

    } else {
      // The failure could be caused by either references. Record the outcome of both.
      dir_ref->expectResult(dir_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
    }
  });
}

/************************ Socket Operations ************************/

void Thread::_socket(int domain, int type, int protocol) noexcept {
  WARN << "socket(2) not yet implemented. Emulating as an anonymous file.";

  finishSyscall([=](long rc) {
    resume();

    if (rc >= 0) {
      auto ref = _build.file(_process->getCommand(), 0600);
      _process->addFD(rc, ref, true, (type & SOCK_CLOEXEC) == SOCK_CLOEXEC);
    }
  });
}

void Thread::_bind(int sockfd, const struct sockaddr* addr, socklen_t addrlen) noexcept {
  WARN << "bind(2) not yet implemented. Ignoring for now.";
  resume();
}

void Thread::_socketpair(int domain, int type, int protocol, int sv[2]) noexcept {
  if (domain == AF_UNIX) {
    finishSyscall([=](long rc) {
      resume();

      if (rc == 0) {
        // Read the file descriptors
        int sock1_fd = readData((uintptr_t)sv);
        int sock2_fd = readData((uintptr_t)sv + sizeof(int));

        // Are the sockets closed on exec?
        bool cloexec = (type & SOCK_CLOEXEC) == SOCK_CLOEXEC;

        // Create an anonymous file to represent the socket
        auto ref = _build.file(_process->getCommand(), 0600);

        // Add the file descriptors
        _process->addFD(sock1_fd, ref, true, cloexec);
        _process->addFD(sock2_fd, ref, true, cloexec);
      }
    });
  } else {
    FAIL << "socketpair(2) for non-UNIX sockets is not implemented.";
  }
}

/************************ Process State Operations ************************/

void Thread::_chdir(string filename) noexcept {
  LOGF(trace, "{}: chdir(\"{}\")", this, filename);

  finishSyscall([=](long rc) {
    resume();

    // Update the current working directory if the chdir call succeeded
    if (rc == 0) {
      _process->setWorkingDir(makeAccess(filename, AccessFlags{.x = true}));
    }
  });
}

void Thread::_chroot(string filename) noexcept {
  LOGF(trace, "{}: chroot(\"{}\")", this, filename);
  FAIL << "Builds that use chroot are not supported.";
}

void Thread::_pivot_root(string new_root, string put_old) noexcept {
  LOGF(trace, "{}: pivot_root(\"{}\", \"{}\")", this, new_root, put_old);
  FAIL << "Builds that use pivot_root are not supported.";
}

void Thread::_fchdir(int fd) noexcept {
  LOGF(trace, "{}: fchdir({})", this, fd);

  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Get the path to the artifact this descriptor references
      const auto& descriptor = _process->getFD(fd);
      auto a = descriptor.getRef()->as<Access>();

      // Make sure there really is a path
      ASSERT(a) << "fchdir to an artifact with no path should not succeed";

      // Update the working directory
      _process->setWorkingDir(a);
    }
  });
}

void Thread::_execveat(int dfd, string filename, vector<string> args, vector<string> env) noexcept {
  LOGF(trace, "{}: execveat({}, \"{}\", [\"{}\"])", this, dfd, filename, fmt::join(args, "\", \""));

  // The parent command needs execute access to the exec-ed path
  auto exe_ref = makeAccess(filename, AccessFlags{.x = true}, dfd);

  // Finish the exec syscall and resume
  finishSyscall([=](long rc) {
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

    ASSERT(exe_ref->isResolved()) << "Executable file failed to resolve";

    // Update the process state with the new executable
    _process->exec(exe_ref, args, env);

    // The child command depends on the contents of its executable. First, we need to know what
    // the actual executable is. Read /proc/<pid>/exe to find it
    auto real_exe_path = readlink("/proc/" + std::to_string(_process->getID()) + "/exe");

    // Now make the reference and expect success
    auto child_exe_ref = makeAccess(real_exe_path, AccessFlags{.r = true});
    child_exe_ref->expectResult(SUCCESS);

    ASSERT(child_exe_ref->isResolved()) << "Failed to locate artifact for executable file";

    // The child command depends on the contents of the executable
    _build.matchContent(_process->getCommand(), child_exe_ref);
  });
}

void Thread::_wait4(pid_t pid, int* wstatus, int options) noexcept {
  LOGF(trace, "{}: wait4({}, {}, {})", this, pid, (void*)wstatus, options);

  finishSyscall([=](long rc) {
    int status = 0;
    if (wstatus != nullptr) status = readData<int>((uintptr_t)wstatus);

    resume();

    // If the syscall failed or returned immediately after WNOHANG, stop processing
    if (rc <= 0) return;

    // Get the process that was returned
    auto exited = _tracer.getExited(rc);

    ASSERT(exited) << "wait4 syscall returned an untracked PID " << rc;

    if (exited->getCommand() != _process->getCommand()) {
      _build.exit(exited->getCommand(), WEXITSTATUS(status));
      if (WIFEXITED(status)) {
        _build.join(_process->getCommand(), exited->getCommand(), WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        // TODO: Should we encode termination by signal in some other way?
        // (yes, "some other way")
        _build.join(_process->getCommand(), exited->getCommand(), WEXITSTATUS(status));
      }
    }
  });
}

void Thread::_waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept {
  LOGF(trace, "{}: waitid(...)", this);
  FAIL << "waitid syscall is not handled yet";
  resume();
}
