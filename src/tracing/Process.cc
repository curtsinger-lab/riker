#include "Process.hh"

#include <functional>
#include <memory>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "build/Build.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/RefResult.hh"
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
void Process::setWorkingDir(shared_ptr<RefResult> ref) noexcept {
  ASSERT(ref->getResult()->getPath(false).has_value())
      << "Cannot set working directory to an artifact without a committed path";
  _cwd = ref;
}

// Get a file descriptor entry
FileDescriptor& Process::getFD(int fd) noexcept {
  ASSERT(_fds.find(fd) != _fds.end())
      << "Attempted to access an unknown fd " << fd << " in " << this;
  return _fds.at(fd);
}

// Add a file descriptor entry
FileDescriptor& Process::addFD(int fd,
                               shared_ptr<RefResult> ref,
                               AccessFlags flags,
                               bool cloexec) noexcept {
  if (auto iter = _fds.find(fd); iter != _fds.end()) {
    WARN << "Overwriting an existing fd " << fd << " in " << this;
    _fds.erase(iter);
  }

  auto [iter, added] = _fds.emplace(fd, FileDescriptor(ref, flags, cloexec));
  return iter->second;
}

// Close a file descriptor
void Process::closeFD(int fd) noexcept {
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    LOG(trace) << "Closing an unknown file descriptor " << fd << " in " << this;
  } else {
    _fds.erase(iter);
  }
}

// Remove a file descriptor entry if it exists
void Process::tryCloseFD(int fd) noexcept {
  _fds.erase(fd);
}

// The process is executing a new file
void Process::exec(shared_ptr<RefResult> exe_ref,
                   vector<string> args,
                   vector<string> env) noexcept {
  // Build a map of the initial file descriptors for the child command
  // As we build this map, keep track of which file descriptors have to be erased from the
  // process' current map of file descriptors.
  map<int, FileDescriptor> initial_fds;
  list<int> to_erase;

  for (const auto& [index, fd] : _fds) {
    if (fd.isCloexec()) {
      to_erase.push_back(index);
    } else {
      initial_fds.emplace(index, FileDescriptor(fd.getRef(), fd.getFlags()));
    }
  }
  for (int index : to_erase) {
    _fds.erase(index);
  }

  // Create the child command
  auto child = make_shared<Command>(exe_ref, args, initial_fds, _cwd, _root);

  // Inform the build of the launch action
  _build.traceLaunch(_command, child);

  // This process is now running the child
  _command = child;

  // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
  // that participate in that command. This will require some extra bookkeeping. For now, we
  // over-approximate the set of commands that have a file mmapped.
}

shared_ptr<RefResult> Thread::makePathRef(fs::path p, AccessFlags flags, int at) noexcept {
  // Absolute paths are resolved relative to the process' current root
  if (p.is_absolute()) {
    return _build.tracePathRef(getCommand(), _process->getRoot(), p.relative_path(), flags);
  }

  // Handle the special CWD file descriptor to resolve relative to cwd
  if (at == AT_FDCWD) {
    return _build.tracePathRef(getCommand(), _process->getWorkingDir(), p.relative_path(), flags);
  }

  // The path is resolved relative to some file descriptor
  return _build.tracePathRef(getCommand(), _process->getFD(at).getRef(), p.relative_path(), flags);
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
  int rc = ptrace(PTRACE_CONT, _tid, nullptr, 0);
  FAIL_IF(rc == -1 && errno != ESRCH) << "Failed to resume child: " << ERR;
}

void Thread::finishSyscall(function<void(long)> handler) noexcept {
  ASSERT(!_post_syscall_handler) << "Process already has an unexecuted post-syscall handler";

  // Set the post-syscall handler
  _post_syscall_handler = handler;

  // Allow the tracee to resume until its syscall finishes
  int rc = ptrace(PTRACE_SYSCALL, _tid, nullptr, 0);
  FAIL_IF(rc == -1 && errno != ESRCH) << "Failed to resume child: " << ERR;
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
    // can be smaller. Advance position (the index into the output array) by the number of
    // complete elements read.
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

/// A pretty-printing wrapper for the directory file descriptor passed to *at syscalls
class dfd_printer {
 public:
  dfd_printer(int fd) : _fd(fd) {}

  friend ostream& operator<<(ostream& o, const dfd_printer& p) noexcept {
    if (p._fd == AT_FDCWD) {
      return o << "AT_FDCWD";
    } else {
      return o << p._fd;
    }
  }

 private:
  int _fd;
};

/// A pretty-printing wrapper for flags passed to openat
class openat_flags_printer {
 public:
  openat_flags_printer(int flags) : _flags(flags) {}

  friend ostream& operator<<(ostream& o, const openat_flags_printer& f) noexcept {
    bool noflag = true;

    // decode O_RDWR, O_RDONLY, O_WRONLY. Check O_RDWR first in case O_RDWR == O_RDONLY | O_WRONLY
    if ((f._flags & O_RDWR) == O_RDWR) {
      o << "O_RDWR";
      noflag = false;
    } else if ((f._flags & O_WRONLY) == O_WRONLY) {
      o << "O_WRONLY";
      noflag = false;
    } else if ((f._flags & O_RDONLY) == O_RDONLY) {
      o << "O_RDONLY";
      noflag = false;
    }

    // pretty printer
    auto dec = [&](int flag, const char* fstr) {
      if ((f._flags & flag) == flag) {
        if (!noflag) o << "|";
        o << fstr;
        noflag = false;
      }
    };

    // decode the rest
    dec(O_CREAT, "O_CREAT");
    dec(O_EXCL, "O_EXCL");
    dec(O_NOCTTY, "O_NOCTTY");
    dec(O_TRUNC, "O_TRUNC");
    dec(O_DIRECTORY, "O_DIRECTORY");
    dec(O_NOFOLLOW, "O_NOFOLLOW");
    dec(O_CLOEXEC, "O_CLOEXEC");
    dec(O_TMPFILE, "O_TMPFILE");

    // append flags in octal
    o << fmt::format(" ({:o})", f._flags);

    return o;
  }

 private:
  int _flags;
};

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

/************************* File Opening, Creation, and Closing ************************/

void Thread::_openat(int dfd, string filename, int flags, mode_t mode) noexcept {
  LOGF(trace, "{}: openat({}, \"{}\", {}, {:o})", this, dfd_printer(dfd), filename,
       openat_flags_printer(flags), mode);

  // Get a reference from the given path
  // Attempt to get an artifact using this reference *BEFORE* running the syscall.
  // This will ensure the environment knows whether or not this artifact is created
  auto ref_flags = AccessFlags::fromOpen(flags, mode);
  auto ref = makePathRef(filename, ref_flags, dfd);

  // Allow the syscall to finish
  finishSyscall([=](long fd) {
    // Let the process continue
    resume();

    // Check whether the openat call succeeded or failed
    if (fd >= 0) {
      // The command observed a successful openat, so add this predicate to the command log
      _build.traceExpectResult(getCommand(), ref, SUCCESS);

      ASSERT(ref->getResult()) << "Failed to locate artifact for opened file: " << filename
                               << " (received " << ref->getResult() << " from emulator)";

      // Is this new descriptor closed on exec?
      bool cloexec = ((flags & O_CLOEXEC) == O_CLOEXEC);

      // If the O_TMPFILE flag was passed, this call created a reference to an anonymous file
      if ((flags & O_TMPFILE) == O_TMPFILE) {
        auto anon_ref = _build.traceFileRef(getCommand(), mode);

        // Record the reference in the process' file descriptor table
        _process->addFD(fd, anon_ref, ref_flags, cloexec);

      } else {
        // If the file is truncated by the open call, set the contents in the artifact
        if (ref_flags.truncate) {
          auto written = make_shared<FileVersion>(FileFingerprint::makeEmpty());
          _build.traceUpdateContent(getCommand(), ref, written);
        }

        // Record the reference in the correct location in this process' file descriptor table
        _process->addFD(fd, ref, ref_flags, cloexec);
      }

    } else {
      // The command observed a failed openat, so add the error predicate to the command log
      // Negate fd because syscalls return negative errors
      _build.traceExpectResult(getCommand(), ref, -fd);
    }
  });
}

void Thread::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) noexcept {
  LOGF(trace, "{}: mknodat({}, \"{}\", {:o}, {})", this, dfd_printer(dfd), filename, mode, dev);

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
    if (rc == 0) {
      LOGF(trace, "{}: closing FD {}", this, fd);
      _process->closeFD(fd);
    } else {
      LOGF(trace, "{}: close({}) returned error {}", this, rc, errors[-rc]);
    }
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
    auto [read_ref, write_ref] = _build.tracePipeRef(getCommand());
    ASSERT(read_ref->getResult() && write_ref->getResult()) << "Failed to get artifact for pipe";

    // Check if this pipe is closed on exec
    bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

    // Fill in the file descriptor entries
    _process->addFD(read_pipefd, read_ref, AccessFlags{.r = true}, cloexec);
    _process->addFD(write_pipefd, write_ref, AccessFlags{.w = true}, cloexec);
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
      _process->addFD(newfd, descriptor.getRef(), descriptor.getFlags(), false);
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
      _process->addFD(rc, descriptor.getRef(), descriptor.getFlags(), cloexec);
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
  LOGF(trace, "{}: faccessat({}, \"{}\", {:o}, {})", this, dfd_printer(dirfd), pathname, mode,
       flags);

  // Finish the syscall so we can see its result
  finishSyscall([=](long rc) {
    // Resume the process' execution
    resume();

    // Create a reference
    auto ref = makePathRef(pathname, AccessFlags::fromAccess(mode, flags), dirfd);

    // Record the outcome of the reference
    _build.traceExpectResult(getCommand(), ref, -rc);

    if (rc == 0) {
      if (!ref->getResult()) WARN << "Failed to resolve reference " << ref;
      // Don't abort here because the dodo self-build accesses /proc/self.
      // We need to fix these references for real at some point.
    }
  });
}

void Thread::_fstatat(int dirfd, string pathname, struct stat* statbuf, int flags) noexcept {
  LOGF(trace, "{}: fstatat({}, \"{}\", {}, {})", this, dfd_printer(dirfd), pathname, (void*)statbuf,
       flags);

  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if ((flags & AT_EMPTY_PATH) == AT_EMPTY_PATH) {
    finishSyscall([=](long rc) {
      resume();

      if (rc == 0) {
        // This is essentially an fstat call
        // Record the dependency on metadata
        _build.traceMatchMetadata(getCommand(), _process->getFD(dirfd).getRef());
      } else {
        WARN << "fstatat AT_EMPTY_PATH failed ¯\\_(ツ)_/¯";
        // do nothing.
      }
    });
  } else {
    // Finish the syscall to see if the reference succeeds
    finishSyscall([=](long rc) {
      resume();

      if (rc == 0) {
        // The stat succeeded
        auto ref = makePathRef(pathname, {}, dirfd);
        _build.traceExpectResult(getCommand(), ref, SUCCESS);
        ASSERT(ref->getResult()) << "Unable to locate artifact for stat-ed file " << ref;

        // Record the dependence on the artifact's metadata
        _build.traceMatchMetadata(getCommand(), ref);

      } else if (rc == -EACCES || rc == -ENOENT || rc == -ENOTDIR) {
        // The stat failed with a filesystem-related error
        auto ref = makePathRef(pathname, {}, dirfd);
        _build.traceExpectResult(getCommand(), ref, -rc);
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
  _build.traceMatchMetadata(getCommand(), descriptor.getRef());

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.traceUpdateMetadata(getCommand(), descriptor.getRef());
  });
}

void Thread::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flags) noexcept {
  LOGF(trace, "{}: fchownat({}, \"{}\", {}, {}, {})", this, dfd_printer(dfd), filename, user, group,
       flags);

  // Make a reference to the file that will be chown-ed.
  bool nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
  auto ref = makePathRef(filename, AccessFlags{.nofollow = nofollow}, dfd);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->getResult()) {
    _build.traceMatchMetadata(getCommand(), ref);
  }

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      _build.traceExpectResult(getCommand(), ref, SUCCESS);

      ASSERT(ref->getResult()) << "Failed to get artifact";

      // We've now set the artifact's metadata
      _build.traceUpdateMetadata(getCommand(), ref);

    } else {
      // No. Record the failure
      _build.traceExpectResult(getCommand(), ref, -rc);
    }
  });
}

void Thread::_fchmod(int fd, mode_t mode) noexcept {
  LOGF(trace, "{}: fchmod({}, {:o})", this, fd, mode);

  // Get the file descriptor entry
  auto& descriptor = _process->getFD(fd);

  // The command depends on the old metadata
  _build.traceMatchMetadata(getCommand(), descriptor.getRef());

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.traceUpdateMetadata(getCommand(), descriptor.getRef());
  });
}

void Thread::_fchmodat(int dfd, string filename, mode_t mode, int flags) noexcept {
  LOGF(trace, "{}: fchmodat({}, \"{}\", {:o}, {})", this, dfd_printer(dfd), filename, mode, flags);

  // Make a reference to the file that will be chmod-ed.
  bool nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
  auto ref = makePathRef(filename, AccessFlags{.nofollow = nofollow}, dfd);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->getResult()) {
    _build.traceMatchMetadata(getCommand(), ref);
  }

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      _build.traceExpectResult(getCommand(), ref, SUCCESS);

      ASSERT(ref->getResult()) << "Failed to get artifact";

      // We've now set the artifact's metadata
      _build.traceUpdateMetadata(getCommand(), ref);

    } else {
      // No. Record the failure
      _build.traceExpectResult(getCommand(), ref, -rc);
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
    _build.traceMatchContent(getCommand(), descriptor.getRef());
  });
}

void Thread::_write(int fd) noexcept {
  LOGF(trace, "{}: write({})", this, fd);

  // Get the descriptor
  const auto& descriptor = _process->getFD(fd);

  // Record our dependency on the old contents of the artifact
  _build.traceMatchContent(getCommand(), descriptor.getRef());

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the write syscall failed, there's no need to log a write
    if (rc == -1) return;

    // Record the update to the artifact contents
    _build.traceUpdateContent(getCommand(), descriptor.getRef());
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
    _build.traceMatchContent(getCommand(), descriptor.getRef());

    // If the mapping is writable, and the file was opened in write mode, the command
    // is also effectively setting the contents of the file.
    bool writable = (prot & PROT_WRITE) && descriptor.isWritable();
    if (writable) {
      _build.traceUpdateContent(getCommand(), descriptor.getRef());
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
  auto ref = makePathRef(pathname, AccessFlags{.w = true});

  // If length is non-zero, we depend on the previous contents
  // This only applies if the artifact exists
  if (length > 0 && ref->getResult()) {
    _build.traceMatchContent(getCommand(), ref);
  }

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // Record the outcome of the reference
    _build.traceExpectResult(getCommand(), ref, -rc);

    // Did the call succeed?
    if (rc == 0) {
      // Make sure the artifact actually existed
      ASSERT(ref->getResult()) << "Failed to get artifact for truncated file";

      // Record the update to the artifact contents
      _build.traceUpdateContent(getCommand(), ref);
    }
  });
}

void Thread::_ftruncate(int fd, long length) noexcept {
  LOGF(trace, "{}: ftruncate({}, {})", this, fd, length);

  // Get the descriptor
  const auto& descriptor = _process->getFD(fd);

  // If length is non-zero, this is a write so we depend on the previous contents
  if (length > 0) {
    _build.traceMatchContent(getCommand(), descriptor.getRef());
  }

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Record the update to the artifact contents
      _build.traceUpdateContent(getCommand(), descriptor.getRef());
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
  _build.traceMatchContent(getCommand(), out_desc.getRef());

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // The command has now read the input file, so it depends on the contents there
    _build.traceMatchContent(getCommand(), in_desc.getRef());

    // The command has now set the contents of the output file
    _build.traceUpdateContent(getCommand(), out_desc.getRef());
  });
}

/************************ Directory Operations ************************/

void Thread::_mkdirat(int dfd, string pathname, mode_t mode) noexcept {
  LOGF(trace, "{}: mkdirat({}, \"{}\", {:o})", this, dfd_printer(dfd), pathname, mode);

  auto full_path = fs::path(pathname);

  // Strip a trailing slash from pathname if it has one
  if (full_path.filename().empty()) {
    WARN << "Removing trailing slash from " << full_path;
    full_path = full_path.parent_path();
  }

  auto parent_path = full_path.parent_path();
  auto entry = full_path.filename();

  // Make a reference to the parent directory where the new directory will be added
  auto parent_ref = makePathRef(parent_path, AccessFlags{.w = true}, dfd);

  // Make a reference to the new directory entry that will be created
  auto entry_ref = makePathRef(full_path, AccessFlags{}, dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Write access to the parent directory must succeed
      _build.traceExpectResult(getCommand(), parent_ref, SUCCESS);

      // The entry must not exist prior to this call
      _build.traceExpectResult(getCommand(), entry_ref, ENOENT);

      // Make a directory reference to get a new artifact
      auto dir_ref = _build.traceDirRef(getCommand(), mode);

      // Link the directory into the parent dir
      _build.traceAddEntry(getCommand(), parent_ref, entry, dir_ref);

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      _build.traceExpectResult(getCommand(), parent_ref, parent_ref->getResult());
      _build.traceExpectResult(getCommand(), entry_ref, entry_ref->getResult());
    }
  });
}

void Thread::_renameat2(int old_dfd,
                        string old_name,
                        int new_dfd,
                        string new_name,
                        int flags) noexcept {
  LOGF(trace, "{}: renameat({}, \"{}\", {}, \"{}\", {})", this, dfd_printer(old_dfd), old_name,
       dfd_printer(new_dfd), new_name, flags);

  // Break the path to the existing file into directory and entry parts
  auto old_path = fs::path(old_name);
  auto old_dir = old_path.parent_path();
  auto old_entry = old_path.filename();

  // Make references to the old directory and entry
  auto old_dir_ref = makePathRef(old_dir, AccessFlags{.w = true}, old_dfd);

  auto old_entry_ref = makePathRef(old_path, AccessFlags{.nofollow = true}, old_dfd);

  // Break the path to the new file into directory and entry parts
  auto new_path = fs::path(new_name);
  auto new_dir = new_path.parent_path();
  auto new_entry = new_path.filename();

  // Make a reference to the new directory
  auto new_dir_ref = makePathRef(new_dir, AccessFlags{.w = true}, new_dfd);

  // If either RENAME_EXCHANGE or RENAME_NOREPLACE is specified, make a reference to the new entry
  shared_ptr<RefResult> new_entry_ref;
  if ((flags & RENAME_EXCHANGE) || (flags & RENAME_NOREPLACE)) {
    new_entry_ref = makePathRef(new_path, AccessFlags{.nofollow = true}, new_dfd);
  }

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // The accesses to the old directory and entry must have succeeded
      _build.traceExpectResult(getCommand(), old_dir_ref, SUCCESS);
      _build.traceExpectResult(getCommand(), old_entry_ref, SUCCESS);

      // Unlink the old entry
      _build.traceRemoveEntry(getCommand(), old_dir_ref, old_entry, old_entry_ref);

      // The access to the new directory must also have succeeded
      _build.traceExpectResult(getCommand(), new_dir_ref, SUCCESS);

      // Is this an exchange or noreplace option?
      if (flags & RENAME_EXCHANGE) {
        // This is an exchange, so the new_entry_ref must exist
        _build.traceExpectResult(getCommand(), new_entry_ref, SUCCESS);

        // Unlink the new entry
        _build.traceRemoveEntry(getCommand(), new_dir_ref, new_entry, new_entry_ref);

      } else if (flags & RENAME_NOREPLACE) {
        // This is a noreplace rename, so new_entry_ref must not exist
        _build.traceExpectResult(getCommand(), new_entry_ref, ENOENT);
      }

      // Link into the new entry
      _build.traceAddEntry(getCommand(), new_dir_ref, new_entry, old_entry_ref);

      // If this is an exchange, we also have to perform the swapped link
      if (flags & RENAME_EXCHANGE) {
        _build.traceAddEntry(getCommand(), old_dir_ref, old_entry, new_entry_ref);
      }
    } else {
      // The syscall failed. Be conservative and save the result of all references. If any of them
      // change, that COULD change the syscall outcome.
      _build.traceExpectResult(getCommand(), old_dir_ref, old_dir_ref->getResult());
      _build.traceExpectResult(getCommand(), old_entry_ref, old_entry_ref->getResult());
      _build.traceExpectResult(getCommand(), new_dir_ref, new_dir_ref->getResult());
      if (new_entry_ref) {
        _build.traceExpectResult(getCommand(), new_entry_ref, new_entry_ref->getResult());
      }
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
      _build.traceMatchContent(getCommand(), descriptor.getRef());
    }
  });
}

/************************ Link and Symlink Operations ************************/

void Thread::_linkat(int old_dfd, string oldpath, int new_dfd, string newpath, int flags) noexcept {
  LOGF(trace, "{}: linkat({}, \"{}\", {}, \"{}\", {})", this, dfd_printer(old_dfd), oldpath,
       dfd_printer(new_dfd), newpath, flags);

  // The newpath string is the path to the new link. Split that into the directory and entry.
  auto link_path = fs::path(newpath);
  auto dir_path = link_path.parent_path();
  auto entry = link_path.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makePathRef(dir_path, AccessFlags{.w = true}, new_dfd);

  // Get a reference to the link we are creating
  auto entry_ref = makePathRef(link_path, AccessFlags{}, new_dfd);

  // Get a reference to the artifact we are linking into the directory
  AccessFlags target_flags = {.nofollow = true};
  if (flags & AT_SYMLINK_FOLLOW) target_flags.nofollow = false;

  auto target_ref = makePathRef(oldpath, target_flags, old_dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc == 0) {
      // Write access to the directory must succeed
      _build.traceExpectResult(getCommand(), dir_ref, SUCCESS);

      // The link must not exist prior to this call
      _build.traceExpectResult(getCommand(), entry_ref, ENOENT);

      // The reference to the link target must succeed
      _build.traceExpectResult(getCommand(), target_ref, SUCCESS);

      // Record the link operation
      _build.traceAddEntry(getCommand(), dir_ref, entry, target_ref);

    } else {
      // The failure could be caused by the dir_ref, entry_ref, or target_ref. To be safe, just
      // record the result of resolving each of them.
      _build.traceExpectResult(getCommand(), dir_ref, dir_ref->getResult());
      _build.traceExpectResult(getCommand(), entry_ref, entry_ref->getResult());
      _build.traceExpectResult(getCommand(), target_ref, target_ref->getResult());
    }
  });
}

void Thread::_symlinkat(string target, int dfd, string newpath) noexcept {
  LOGF(trace, "{}: symlinkat(\"{}\", {}, \"{}\")", this, target, dfd_printer(dfd), newpath);

  // The newpath string is the path to the new link. Split that into the directory and entry.
  auto link_path = fs::path(newpath);
  auto dir_path = link_path.parent_path();
  auto entry = link_path.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makePathRef(dir_path, AccessFlags{.w = true}, dfd);

  // Get a reference to the link we are creating
  auto entry_ref = makePathRef(link_path, AccessFlags{}, dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Write access to the directory must succeed
      _build.traceExpectResult(getCommand(), dir_ref, SUCCESS);

      // The link must not exist prior to this call
      _build.traceExpectResult(getCommand(), entry_ref, ENOENT);

      // Make a symlink reference to get a new artifact
      auto symlink_ref = _build.traceSymlinkRef(getCommand(), target);

      // Link the symlink into the directory
      _build.traceAddEntry(getCommand(), dir_ref, entry, symlink_ref);

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      _build.traceExpectResult(getCommand(), dir_ref, dir_ref->getResult());
      _build.traceExpectResult(getCommand(), entry_ref, entry_ref->getResult());
    }
  });
}

void Thread::_readlinkat(int dfd, string pathname) noexcept {
  LOGF(trace, "{}: readlinkat({}, \"{}\")", this, dfd_printer(dfd), pathname);

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
    auto ref = makePathRef(pathname, AccessFlags{.nofollow = true}, dfd);

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      _build.traceExpectResult(getCommand(), ref, SUCCESS);

      ASSERT(ref->getResult()) << "Failed to get artifact for successfully-read link";

      // We depend on this artifact's contents now
      _build.traceMatchContent(getCommand(), ref);

    } else {
      // No. Record the failure
      _build.traceExpectResult(getCommand(), ref, -rc);
    }
  });
}

void Thread::_unlinkat(int dfd, string pathname, int flags) noexcept {
  LOGF(trace, "{}: unlinkat({}, \"{}\", {})", this, dfd_printer(dfd), pathname, flags);

  // TODO: Make sure pathname does not refer to a directory, unless AT_REMOVEDIR is set

  // Split the pathname into the parent and entry
  auto path = fs::path(pathname);
  auto dir_path = path.parent_path();
  auto entry = path.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makePathRef(dir_path, AccessFlags{.w = true}, dfd);

  // Get a reference to the entry itself
  auto entry_ref = makePathRef(path, AccessFlags{.nofollow = true}, dfd);

  // If this call is removing a directory, depend on the directory contents
  if (entry_ref->getResult()) {
    if (auto dir = entry_ref->getResult()->as<DirArtifact>()) {
      _build.traceMatchContent(getCommand(), entry_ref);
    }
  }

  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc == 0) {
      // Both references must have succeeded
      _build.traceExpectResult(getCommand(), dir_ref, SUCCESS);
      _build.traceExpectResult(getCommand(), entry_ref, SUCCESS);

      // Perform the unlink
      _build.traceRemoveEntry(getCommand(), dir_ref, entry, entry_ref);

    } else {
      // The failure could be caused by either references. Record the outcome of both.
      _build.traceExpectResult(getCommand(), dir_ref, dir_ref->getResult());
      _build.traceExpectResult(getCommand(), entry_ref, entry_ref->getResult());
    }
  });
}

/************************ Socket Operations ************************/

void Thread::_socket(int domain, int type, int protocol) noexcept {
  WARN << "socket(2) not yet implemented. Emulating as an anonymous file.";

  finishSyscall([=](long rc) {
    resume();

    if (rc >= 0) {
      auto ref = _build.traceFileRef(getCommand(), 0600);
      _process->addFD(rc, ref, AccessFlags{.r = true, .w = true},
                      (type & SOCK_CLOEXEC) == SOCK_CLOEXEC);
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

        WARN << "socketpair fds are = {" << sock1_fd << ", " << sock2_fd << "}";

        // Are the sockets closed on exec?
        bool cloexec = (type & SOCK_CLOEXEC) == SOCK_CLOEXEC;

        // Create an anonymous file to represent the socket
        auto ref = _build.traceFileRef(getCommand(), 0600);

        // Add the file descriptors
        _process->addFD(sock1_fd, ref, AccessFlags{.r = true, .w = true}, cloexec);
        _process->addFD(sock2_fd, ref, AccessFlags{.r = true, .w = true}, cloexec);
      }
    });
  } else {
    FAIL << "socketpair(2) for non-UNIX sockets is not implemented.";
  }
}

/************************ Process State Operations ************************/

void Thread::_chdir(string filename) noexcept {
  LOGF(trace, "{}: chdir(\"{}\")", this, filename);

  auto ref = makePathRef(filename, AccessFlags{.x = true});

  finishSyscall([=](long rc) {
    resume();

    _build.traceExpectResult(getCommand(), ref, -rc);

    // Update the current working directory if the chdir call succeeded
    if (rc == 0) {
      _process->setWorkingDir(ref);
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
      // Update the working directory
      _process->setWorkingDir(_process->getFD(fd).getRef());
    }
  });
}

void Thread::_fork() noexcept {
  LOGF(trace, "{}: fork()", this);

  finishSyscall([=](long rc) {
    resume();

    LOGF(trace, "{}: finished fork(), returned {}", this, rc);
  });
}

void Thread::_clone(void* fn, void* stack, int flags) noexcept {
  LOGF(trace, "{}: clone({}, {}, 0x{:x})", this, fn, stack, (unsigned int)flags);

  finishSyscall([=](long rc) {
    LOGF(trace, "{}: finished clone(), returned {}", this, rc);
    resume();
  });
}

void Thread::_exit(int status) noexcept {
  LOGF(trace, "{}: exit({})", this, status);
  resume();
}

void Thread::_exit_group(int status) noexcept {
  LOGF(trace, "{}: exit_group({})", this, status);
  resume();
}

void Thread::_execveat(int dfd, string filename, vector<string> args, vector<string> env) noexcept {
  LOGF(trace, "{}: execveat({}, \"{}\", [\"{}\"])", this, dfd_printer(dfd), filename,
       fmt::join(args, "\", \""));

  // The parent command needs execute access to the exec-ed path
  auto exe_ref = makePathRef(filename, AccessFlags{.x = true}, dfd);

  // Finish the exec syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // Not sure why, but exec returns -38 on success.
    // If we see something else, handle the error
    if (rc != -38) {
      // Failure! Record a failed reference. Negate rc because syscalls return negative errors
      _build.traceExpectResult(getCommand(), exe_ref, -rc);
      return;
    }

    // If we reached this point, the executable reference was okay
    _build.traceExpectResult(getCommand(), exe_ref, SUCCESS);

    ASSERT(exe_ref->getResult()) << "Executable file failed to resolve";

    // Update the process state with the new executable
    _process->exec(exe_ref, args, env);

    // The child command depends on the contents of its executable. First, we need to know what
    // the actual executable is. Read /proc/<pid>/exe to find it
    auto real_exe_path = readlink("/proc/" + std::to_string(_process->getID()) + "/exe");

    // Now make the reference and expect success
    auto child_exe_ref = makePathRef(real_exe_path, AccessFlags{.r = true});
    _build.traceExpectResult(getCommand(), child_exe_ref, SUCCESS);

    ASSERT(child_exe_ref->getResult()) << "Failed to locate artifact for executable file";

    // The child command depends on the contents of the executable
    _build.traceMatchContent(getCommand(), child_exe_ref);
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

    if (exited->getCommand() != getCommand()) {
      _build.traceExit(exited->getCommand(), WEXITSTATUS(status));
      if (WIFEXITED(status)) {
        _build.traceJoin(getCommand(), exited->getCommand(), WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        // TODO: Should we encode termination by signal in some other way?
        // (yes, "some other way")
        _build.traceJoin(getCommand(), exited->getCommand(), WEXITSTATUS(status));
      }
    }
  });
}

void Thread::_waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept {
  LOGF(trace, "{}: waitid(...)", this);
  FAIL << "waitid syscall is not handled yet";
  resume();
}
