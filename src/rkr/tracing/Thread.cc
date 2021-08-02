#include "Thread.hh"

#include <cerrno>
#include <climits>
#include <cstdlib>
#include <filesystem>
#include <functional>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

#include <fmt/format.h>
#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "data/AccessFlags.hh"
#include "data/IRSink.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "tracing/Flags.hh"
#include "tracing/SyscallTable.hh"
#include "tracing/Tracer.hh"
#include "util/log.hh"
#include "util/options.hh"
#include "util/wrappers.hh"
#include "versions/MetadataVersion.hh"

using std::function;
using std::optional;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

// Traced entry to a system call through the provided shared memory channel
void Thread::syscallEntryChannel(ssize_t channel) noexcept {
  ASSERT(_channel == -1) << this << " is already using a shared memory channel";
  _channel = channel;

  auto& entry = SyscallTable::get(Tracer::getSyscallNumber(_channel));

  if (options::syscall_stats) {
    Tracer::syscall_counts[string(entry.getName()) + " (fast)"]++;
    Tracer::fast_syscall_count++;
  }

  LOG(trace) << this << " handling " << entry.getName() << " entry via shared memory channel";

  entry.runHandler(*this, Tracer::getRegisters(_channel), _channel);

  _channel = -1;
}

// Traced exit from a system call through the provided shared memory channel
void Thread::syscallExitChannel(ssize_t channel) noexcept {
  ASSERT(_channel == -1) << this << " is already using a shared memory channel";
  _channel = channel;

  ASSERT(!_post_syscall_handlers.empty())
      << "Stopped on syscall exit with no available post-syscall handlers";

  LOG(trace) << this << " handling "
             << SyscallTable::get(Tracer::getSyscallNumber(_channel)).getName()
             << " exit via shared memory channel";

  // Run the post-syscall handler
  _post_syscall_handlers.top()(Tracer::getSyscallResult(_channel));

  // Remove the used post-syscall handler
  _post_syscall_handlers.pop();

  _channel = -1;
}

void Thread::syscallExitPtrace() noexcept {
  ASSERT(!_post_syscall_handlers.empty()) << "Thread does not have a post-syscall handler";

  // Clear errno so we can check for errors
  errno = 0;

  // Now extract the return code from the syscall.
  long result = ptrace(PTRACE_PEEKUSER, _tid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  // Run the handler and remove it from the stack
  _post_syscall_handlers.top()(result);
  _post_syscall_handlers.pop();
}

fs::path Thread::getPath(at_fd fd) const noexcept {
  if (fd.isCWD()) {
    auto cwd_ref = _process->getWorkingDir();
    auto cwd = getCommand()->getRef(cwd_ref)->getArtifact()->getPath();
    if (cwd.has_value()) return cwd.value();
  } else {
    auto fd_ref = _process->getFD(fd.getFD());
    auto path = getCommand()->getRef(fd_ref)->getArtifact()->getPath();
    if (path.has_value()) return path.value();
  }
  return "<no path>";
}

Ref::ID Thread::makePathRef(fs::path p, AccessFlags flags, at_fd at) noexcept {
  // Absolute paths are resolved relative to the process' current root
  if (p.is_absolute()) {
    // HACK: Remove the O_EXCL flag when creating files in /tmp
    if (p.string().find("/tmp/") == 0) flags.exclusive = false;

    auto ref = getCommand()->nextRef();
    _build.pathRef(getCommand(), _process->getRoot(), p.relative_path(), flags, ref);
    return ref;
  }

  // Handle the special CWD file descriptor to resolve relative to cwd
  if (at.isCWD()) {
    auto ref = getCommand()->nextRef();
    _build.pathRef(getCommand(), _process->getWorkingDir(), p.relative_path(), flags, ref);
    return ref;
  }

  // The path is resolved relative to some file descriptor
  auto ref = getCommand()->nextRef();
  _build.pathRef(getCommand(), _process->getFD(at.getFD()), p.relative_path(), flags, ref);
  return ref;
}

user_regs_struct Thread::getRegisters() noexcept {
  if (_channel >= 0) {
    return Tracer::getRegisters(_channel);
  }

  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, _tid, nullptr, &regs))
      << "Failed to get registers (for PID " << _tid << "): " << ERR;
  return regs;
}

void Thread::setRegisters(user_regs_struct& regs) noexcept {
  ASSERT(_channel == -1) << "Cannot set registers when tracing through the shared memory channel";
  FAIL_IF(ptrace(PTRACE_SETREGS, _tid, nullptr, &regs)) << "Failed to set registers: " << ERR;
}

void Thread::resume() noexcept {
  // Is this thread blocked on the shared memory channel?
  if (_channel >= 0) {
    Tracer::channelProceed(_channel, false);
  } else {
    int rc = ptrace(PTRACE_CONT, _tid, nullptr, 0);
    FAIL_IF(rc == -1 && errno != ESRCH) << "Failed to resume child: " << ERR;
  }
}

void Thread::finishSyscall(function<void(long)> handler) noexcept {
  _post_syscall_handlers.push(handler);

  // Is this thread blocked on the shared memory channel?
  if (_channel >= 0) {
    Tracer::channelProceed(_channel, true);

  } else {
    // Allow the tracee to resume until its syscall finishes
    int rc = ptrace(PTRACE_SYSCALL, _tid, nullptr, 0);
    FAIL_IF(rc == -1 && errno != ESRCH) << "Failed to resume child: " << ERR;
  }
}

void Thread::forceExit(int exit_status) noexcept {
  // Is the thread blocked on a shared memory channel?
  if (_channel >= 0) {
    Tracer::channelForceExit(_channel, exit_status);

  } else {
    auto regs = getRegisters();
    regs.SYSCALL_NUMBER = __NR_exit;
    regs.SYSCALL_ARG1 = exit_status;
    setRegisters(regs);
  }

  // Resume the thread so it can actually perform the exit
  resume();
}

unsigned long Thread::getEventMessage() noexcept {
  FAIL_IF(_channel >= 0) << "The getEventMessage function only works for ptrace stops";

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

fs::path Thread::readPath(uintptr_t tracee_pointer) noexcept {
  // Read a string and convert it to an fs::path
  return readString(tracee_pointer);
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

/****************************************************/
/********** System call handling functions **********/
/****************************************************/

/************************* File Opening, Creation, and Closing ************************/

void Thread::_openat(at_fd dfd, fs::path filename, o_flags flags, mode_flags mode) noexcept {
  LOGF(trace, "{}: openat({}={}, {}, {}, {})", this, dfd, getPath(dfd), filename, flags, mode);

  // If the O_CREAT was specified and filename has a trailing slash, the result is EISDIR and we
  // do not need to trace any interaction here
  if (flags.creat() && filename.filename().empty()) {
    resume();
    return;
  }

  // If the provided name is too long, the result is ENAMETOOLONG and we can do nothing
  if (filename.string().length() > NAME_MAX) {
    resume();
    return;
  }

  // Get a reference from the given path
  // Attempt to get an artifact using this reference *BEFORE* running the syscall.
  // This will ensure the environment knows whether or not this artifact is created
  auto ref_flags = AccessFlags::fromOpen(flags, mode, getProcess()->getUmask());
  auto ref_id = makePathRef(filename, ref_flags, dfd);
  auto ref = getCommand()->getRef(ref_id);

  // If this call might truncate the file, call the pre-truncate method on the artifact
  if (ref->isResolved() && ref_flags.truncate) {
    ref->getArtifact()->beforeTruncate(_build, getCommand(), ref_id);
  }

  // Allow the syscall to finish
  finishSyscall([=](long fd) {
    // Let the process continue
    resume();

    // Check whether the openat call succeeded or failed
    if (fd >= 0) {
      WARN_IF(!ref->isResolved()) << "Model Mismatch: failed to locate artifact for opened file: "
                                  << filename << " (received " << ref << " from model)";

      // The command observed a successful openat, so add this predicate to the command log
      _build.expectResult(getCommand(), Scenario::Build, ref_id, SUCCESS);

      // If the O_TMPFILE flag was passed, this call created a reference to an anonymous file
      if (flags.tmpfile()) {
        auto mask = getProcess()->getUmask();
        auto anon_ref_id = getCommand()->nextRef();
        _build.fileRef(getCommand(), mode.getMode() & ~mask, anon_ref_id);

        // Record the reference in the process' file descriptor table
        _process->addFD(_build, fd, anon_ref_id, flags.cloexec());

      } else {
        // If the file is truncated by the open call, set the contents in the artifact
        if (ref_flags.truncate) {
          ref->getArtifact()->afterTruncate(_build, getCommand(), ref_id);
        }

        // Record the reference in the correct location in this process' file descriptor table
        _process->addFD(_build, fd, ref_id, flags.cloexec());
      }

    } else {
      // Negate fd because syscalls return negative errors
      WARN_IF(ref->getResultCode() != -fd)
          << "Model Mismatch: expected openat to return " << getErrorName(ref->getResultCode())
          << ", but actual result is " << getErrorName(-fd);

      // The command observed a failed openat, so add the error predicate to the command log
      _build.expectResult(getCommand(), Scenario::Build, ref_id, ref->getResultCode());
    }
  });
}

void Thread::_mknodat(at_fd dfd, fs::path filename, mode_flags mode, unsigned dev) noexcept {
  LOGF(trace, "{}: mknodat({}={}, {}, {}, {})", this, dfd, getPath(dfd), filename, mode, dev);

  if (mode.isRegularFile()) {
    // Handle regular file creation with openat
    _openat(dfd, filename, o_flags(O_CREAT | O_EXCL), mode);

  } else if (mode.isFIFO()) {
    // Create a named pipe
    auto dir = filename.parent_path();
    auto entry = filename.filename();

    // Create references to the containing directory and entry
    auto dir_ref = makePathRef(dir, WriteAccess, dfd);
    auto entry_ref = makePathRef(filename, NoAccess, dfd);

    finishSyscall([=](long rc) {
      // Resume the blocked thread
      resume();

      if (rc == 0) {
        // Record the outcomes for the two references
        _build.expectResult(getCommand(), Scenario::Build, dir_ref, SUCCESS);
        _build.expectResult(getCommand(), Scenario::Build, entry_ref, ENOENT);

        // Create a pipe
        auto read_end = getCommand()->nextRef();
        auto write_end = getCommand()->nextRef();
        _build.pipeRef(getCommand(), read_end, write_end);

        // Link the pipe into the directory
        _build.addEntry(getCommand(), dir_ref, entry, read_end);

      } else {
        // The syscall failed. Record the outcome of both references
        _build.expectResult(getCommand(), Scenario::Build, dir_ref,
                            getCommand()->getRef(dir_ref)->getResultCode());
        _build.expectResult(getCommand(), Scenario::Build, entry_ref,
                            getCommand()->getRef(entry_ref)->getResultCode());
      }
    });

  } else {
    WARN << "Unsupported use of mknodat";
    resume();
  }
}

void Thread::_close(int fd) noexcept {
  LOGF(trace, "{}: close({})", this, fd);

  // Resume the process
  resume();

  // Try to close the FD
  _process->tryCloseFD(_build, fd);
}

/************************ Pipes ************************/

void Thread::_pipe2(int* fds, o_flags flags) noexcept {
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
    auto read_ref = getCommand()->nextRef();
    auto write_ref = getCommand()->nextRef();
    _build.pipeRef(getCommand(), read_ref, write_ref);

    ASSERT(getCommand()->getRef(read_ref)->isResolved()) << "Failed to resolve pipe reference";
    ASSERT(getCommand()->getRef(write_ref)->isResolved()) << "Failed to resolve pipe reference";

    // Fill in the file descriptor entries
    _process->addFD(_build, read_pipefd, read_ref, flags.cloexec());
    _process->addFD(_build, write_pipefd, write_ref, flags.cloexec());
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
      if (newfd < 0) return;

      // Add the new entry for the duped fd. The cloexec flag is not inherited, so it's always
      // false.
      _process->addFD(_build, newfd, _process->getFD(fd), false);
    });
  } else {
    finishSyscall([=](long rc) {
      resume();
      ASSERT(rc == -EBADF) << "dup of invalid file descriptor did not fail with EBADF";
    });
  }
}

void Thread::_dup3(int oldfd, int newfd, o_flags flags) noexcept {
  LOGF(trace, "{}: dup3({}, {}, {})", this, oldfd, newfd, flags);

  // If newfd and oldfd are equal, dup2/dup3 just checks if the fd is valid. No need to trace.
  if (newfd == oldfd) {
    resume();
    return;
  }

  // dup3 returns the new file descriptor, or error
  // Finish the syscall so we know what file descriptor to add to our table
  if (_process->hasFD(oldfd)) {
    finishSyscall([=](long rc) {
      resume();

      // If the syscall failed, we have nothing more to do
      // Note: this is different than a failed file access. This failure should not be affected
      //       by the state of the filesystem, so we don't have to log it.
      if (rc < 0) return;

      // If there is an existing descriptor entry number newfd, it is silently closed
      _process->tryCloseFD(_build, newfd);

      // Duplicate the file descriptor
      _process->addFD(_build, rc, _process->getFD(oldfd), flags.cloexec());
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
    _dup3(fd, -1, o_flags(O_CLOEXEC));

  } else if (cmd == F_SETFD) {
    resume();
    // Set the cloexec flag using the argument flags
    _process->setCloexec(fd, arg & FD_CLOEXEC);

  } else {
    resume();
    // Some other operation we do not need to handle
    // TODO: Filter these stops out with BPF/seccomp
  }
}

/************************ Metadata Operations ************************/

void Thread::_faccessat(at_fd dirfd, fs::path pathname, int mode, at_flags flags) noexcept {
  LOGF(trace, "{}: faccessat({}={}, {}, {}, {})", this, dirfd, getPath(dirfd), pathname, mode,
       flags);

  // Finish the syscall so we can see its result
  finishSyscall([=](long rc) {
    // Resume the process' execution
    resume();

    // Create a reference
    auto ref = makePathRef(pathname, AccessFlags::fromAccess(mode, flags), dirfd);

    // Record the outcome of the reference
    _build.expectResult(getCommand(), Scenario::Build, ref, -rc);

    if (rc == 0) {
      if (!getCommand()->getRef(ref)->isResolved()) WARN << "Failed to resolve reference " << ref;
      // Don't abort here because the riker self-build accesses /proc/self.
      // We need to fix these references for real at some point.
    }
  });
}

void Thread::_fstatat(at_fd dirfd,
                      fs::path pathname,
                      struct stat* statbuf,
                      at_flags flags) noexcept {
  LOGF(trace, "{}: fstatat({}={}, {}, {}, {})", this, dirfd, getPath(dirfd), pathname,
       (void*)statbuf, flags);

  // TODO: Trust the model when running in release mode. Otherwise finish the syscall and validate
  // the result. Other syscalls can use this model too, but fstatat is particularly common.

  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if (flags.empty_path()) {
    // Depend on content so the size field is accurate
    if (_process->hasFD(dirfd.getFD())) {
      auto ref_id = _process->getFD(dirfd.getFD());
      auto ref = getCommand()->getRef(ref_id);
      if (ref->isResolved()) {
        ref->getArtifact()->beforeStat(_build, getCommand(), ref_id);
      }
    }

    /*finishSyscall([=](long rc) {
      resume();

      if (rc == 0) {
        // This is essentially an fstat call
        // Record the dependency on metadata
        _build.traceMatchMetadata(getCommand(), _process->getFD(dirfd.getFD()));
      } else {
        WARN << "fstatat AT_EMPTY_PATH failed ¯\\_(ツ)_/¯";
        // do nothing.
      }
    });*/
    resume();

    auto dir_ref_id = _process->getFD(dirfd.getFD());
    auto dir_ref = getCommand()->getRef(dir_ref_id);
    ASSERT(dir_ref->isResolved()) << "Cannot match metadata for unresolved reference";
    _build.matchMetadata(getCommand(), Scenario::Build, dir_ref_id,
                         dir_ref->getArtifact()->getMetadata(getCommand()));

  } else if (pathname.empty()) {
    // fstatat does not allow empty paths if AT_EMPTY_PATH is not passed. No need to trace the
    // failure.
    resume();
    return;

  } else {
    // Make the reference
    auto ref_id = makePathRef(pathname, AccessFlags::fromAtFlags(flags), dirfd);

    // Depend on content so the size field is accurate
    auto ref = getCommand()->getRef(ref_id);
    if (ref->isResolved()) {
      ref->getArtifact()->beforeStat(_build, getCommand(), ref_id);
    }

    resume();

    _build.expectResult(getCommand(), Scenario::Build, ref_id, ref->getResultCode());

    // Finish the syscall to see if the reference succeeds
    /*finishSyscall([=](long rc) {
      resume();

      if (rc == 0) {
        // The stat succeeded
        _build.expectResult(getCommand(), Scenario::Build, ref_id, SUCCESS);
        ASSERT(getCommand()->getRef(ref_id)->isResolved())
            << "Unable to locate artifact for stat-ed file (" << pathname << ")";

        // Record the dependence on the artifact's metadata
        _build.traceMatchMetadata(getCommand(), ref_id);

      } else if (rc == -EACCES || rc == -ENOENT || rc == -ENOTDIR) {
        // The stat failed with a filesystem-related error
        _build.expectResult(getCommand(), Scenario::Build, ref_id, -rc);
      } else {
        // The stat failed with some other error that doesn't matter to us. We see this in rustc.
      }
    });*/
    if (ref->isResolved()) {
      _build.matchMetadata(getCommand(), Scenario::Build, ref_id,
                           ref->getArtifact()->getMetadata(getCommand()));
    }
  }
}

void Thread::_fchown(int fd, uid_t user, gid_t group) noexcept {
  LOGF(trace, "{}: fchown({}, {}, {})", this, fd, user, group);

  // Get the reference for the given file descriptor
  auto ref_id = _process->getFD(fd);
  auto ref = getCommand()->getRef(ref_id);
  ASSERT(ref->isResolved()) << "Cannot match metadata through an unresolved reference";

  // The command depends on the old metadata
  auto old_metadata = ref->getArtifact()->getMetadata(getCommand());
  _build.matchMetadata(getCommand(), Scenario::Build, ref_id, old_metadata);

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.updateMetadata(getCommand(), ref_id, old_metadata.chown(user, group));
  });
}

void Thread::_fchownat(at_fd dfd,
                       fs::path filename,
                       uid_t user,
                       gid_t group,
                       at_flags flags) noexcept {
  LOGF(trace, "{}: fchownat({}={}, {}, {}, {}, {})", this, dfd, getPath(dfd), filename, user, group,
       flags);

  // If the path is empty but AT_EMPTY_PATH was not passed in, the syscall will fail with no
  // effects
  if (!flags.empty_path() && filename.empty()) {
    resume();
    return;
  }

  // Get a reference to the artifact being chowned
  auto ref_id = makePathRef(filename, AccessFlags::fromAtFlags(flags), dfd);
  auto ref = getCommand()->getRef(ref_id);

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Match the old metadata
      auto old_metadata = ref->getArtifact()->getMetadata(getCommand());
      _build.matchMetadata(getCommand(), Scenario::Build, ref_id, old_metadata);

      // Yes. Record the successful reference
      _build.expectResult(getCommand(), Scenario::Build, ref_id, SUCCESS);

      // We've now set the artifact's metadata
      _build.updateMetadata(getCommand(), ref_id, old_metadata.chown(user, group));

    } else {
      // No. Record the failure
      _build.expectResult(getCommand(), Scenario::Build, ref_id, -rc);
      WARN_IF(ref->isResolved()) << "Model mismatch: failed to chown through resolved reference";
    }
  });
}

void Thread::_fchmod(int fd, mode_flags mode) noexcept {
  LOGF(trace, "{}: fchmod({}, {})", this, fd, mode);

  // Get the file descriptor entry
  auto ref_id = _process->getFD(fd);

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    auto ref = getCommand()->getRef(ref_id);
    ASSERT(ref->isResolved()) << "Cannot match metadata through an unresolved reference";
    auto old_metadata = ref->getArtifact()->getMetadata(getCommand());
    _build.matchMetadata(getCommand(), Scenario::Build, ref_id, old_metadata);

    // The command updates the metadata
    _build.updateMetadata(getCommand(), ref_id, old_metadata.chmod(mode.getMode()));
  });
}

void Thread::_fchmodat(at_fd dfd, fs::path filename, mode_flags mode, at_flags flags) noexcept {
  LOGF(trace, "{}: fchmodat({}={}, {}, {}, {})", this, dfd, getPath(dfd), filename, mode, flags);

  // If the path is empty but AT_EMPTY_PATH was not passed in, the syscall will fail with no
  // effects
  if (!flags.empty_path() && filename.empty()) {
    resume();
    return;
  }

  // Get a reference to the artifact being chmoded
  auto ref_id = makePathRef(filename, AccessFlags::fromAtFlags(flags), dfd);

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      _build.expectResult(getCommand(), Scenario::Build, ref_id, SUCCESS);

      auto ref = getCommand()->getRef(ref_id);
      ASSERT(ref->isResolved()) << "Cannot match metadata through an unresolved reference";

      auto old_metadata = ref->getArtifact()->getMetadata(getCommand());
      _build.matchMetadata(getCommand(), Scenario::Build, ref_id, old_metadata);

      // We've now set the artifact's metadata
      _build.updateMetadata(getCommand(), ref_id, old_metadata.chmod(mode.getMode()));

    } else {
      // No. Record the failure
      _build.expectResult(getCommand(), Scenario::Build, ref_id, -rc);
    }
  });
}

/************************ File Content Operations ************************/

void Thread::_read(int fd) noexcept {
  LOGF(trace, "{}: read({})", this, fd);

  // TODO: Only run after the syscall finishes if the artifact requires it. Pipes generally do,
  // but files do not.

  // Get a reference to the artifact being read
  auto ref_id = _process->getFD(fd);
  const auto& ref = getCommand()->getRef(ref_id);

  // Inform the artifact that we are about to read
  ref->getArtifact()->beforeRead(_build, getCommand(), ref_id);

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    if (rc >= 0) {
      // Inform the artifact that the read succeeded
      ref->getArtifact()->afterRead(_build, getCommand(), ref_id);
    }
  });
}

void Thread::_write(int fd) noexcept {
  LOGF(trace, "{}: write({})", this, fd);

  // Get a reference to the artifact being written
  auto ref_id = _process->getFD(fd);
  const auto& ref = getCommand()->getRef(ref_id);

  // Inform the artifact that we are about to write
  ref->getArtifact()->beforeWrite(_build, getCommand(), ref_id);

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the write syscall failed, there's no need to log a write
    if (rc < 0) return;

    // Inform the artifact that it was written
    ref->getArtifact()->afterWrite(_build, getCommand(), ref_id);
  });
}

void Thread::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) noexcept {
  LOGF(trace, "{}: mmap({})", this, fd);

  // Skip anonymous mappings. We never need to handle these because they only allow communication
  // within a single command.
  // NOTE: The BPF program currently excludes these
  if (fd < 0) {
    LOGF(trace, "{}: skipped anonymous mmap({})", this, fd);
    resume();
    return;
  }

  // Get the file descriptor being mapped
  auto ref_id = _process->getFD(fd);
  const auto& ref = getCommand()->getRef(ref_id);
  bool writable = (prot & PROT_WRITE) && ref->getFlags().w;

  // Inform the mapped artifact that it will by read and possibly written
  ref->getArtifact()->beforeRead(_build, getCommand(), ref_id);
  if (writable) ref->getArtifact()->beforeWrite(_build, getCommand(), ref_id);

  // Run the syscall to find out if the mmap succeeded
  finishSyscall([=](long rc) {
    resume();

    LOGF(trace, "{}: finished mmap({})", this, fd);
    void* result = (void*)rc;

    // If the map failed there's nothing to log
    if (result == MAP_FAILED) {
      resume();
      return;
    }

    // Inform the artifact that it has been read and possibly written
    ref->getArtifact()->afterRead(_build, getCommand(), ref_id);
    if (writable) ref->getArtifact()->afterWrite(_build, getCommand(), ref_id);

    // TODO: we need to track which commands have a given artifact mapped.
    // Any time that artifact is modified, all commands that have it mapped will get an
    // implicit CONTENTS_MATCH line added because they could see the new version.
    // Also, any commands with writable mappings of a file could be setting the contents
    // of the file at any time.
    // Any artifact with multiple mappers, at least one of whom has a writable mapping,
    // creates a cycle. All commands involved in that cycle must be collapsed.
  });
}

void Thread::_truncate(fs::path pathname, long length) noexcept {
  LOGF(trace, "{}: truncate({}, {})", this, pathname, length);

  // Make an access to the reference that will be truncated
  auto ref_id = makePathRef(pathname, WriteAccess);
  const auto& ref = getCommand()->getRef(ref_id);

  // Did the reference resolve to an artifact?
  if (!ref->isResolved()) {
    finishSyscall([=](long rc) {
      resume();
      ASSERT(rc != 0) << "Call to truncate() succeeded, but the reference did not resolve";

      // Record the outcome of the reference
      _build.expectResult(getCommand(), Scenario::Build, ref_id, -rc);
    });

  } else {
    // Is the file being truncated to size zero?
    if (length > 0) {
      // No. Treat this as an ordinary write
      ref->getArtifact()->beforeWrite(_build, getCommand(), ref_id);

    } else {
      // Yes. There is no dependency on the prior contents.
      ref->getArtifact()->beforeTruncate(_build, getCommand(), ref_id);
    }

    finishSyscall([=](long rc) {
      resume();

      // We expect the reference to succeed
      _build.expectResult(getCommand(), Scenario::Build, ref_id, SUCCESS);

      // If the syscall succeeded, finish the write
      if (rc == 0) {
        if (length > 0) {
          ref->getArtifact()->afterWrite(_build, getCommand(), ref_id);
        } else {
          ref->getArtifact()->afterTruncate(_build, getCommand(), ref_id);
        }
      }
    });
  }
}

void Thread::_ftruncate(int fd, long length) noexcept {
  LOGF(trace, "{}: ftruncate({}, {})", this, fd, length);

  // Get the reference to the artifact being written
  auto ref_id = _process->getFD(fd);
  const auto& ref = getCommand()->getRef(ref_id);

  // If length is non-zero, this is a write so we depend on the previous contents
  if (length > 0) {
    ref->getArtifact()->beforeWrite(_build, getCommand(), ref_id);
  } else {
    ref->getArtifact()->beforeTruncate(_build, getCommand(), ref_id);
  }

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Record the update to the artifact contents
      if (length > 0) {
        ref->getArtifact()->afterWrite(_build, getCommand(), ref_id);
      } else {
        ref->getArtifact()->afterTruncate(_build, getCommand(), ref_id);
      }
    }
  });
}

void Thread::_tee(int fd_in, int fd_out) noexcept {
  LOGF(trace, "{}: tee({}, {})", this, fd_in, fd_out);

  // Does the process have the input FD?
  if (_process->hasFD(fd_in)) {
    // Get the descriptors
    auto in_ref_id = _process->getFD(fd_in);
    const auto& in_ref = getCommand()->getRef(in_ref_id);

    auto out_ref_id = _process->getFD(fd_out);
    const auto& out_ref = getCommand()->getRef(out_ref_id);

    // We are abou to read from in_ref and write to out_ref
    in_ref->getArtifact()->beforeRead(_build, getCommand(), in_ref_id);
    out_ref->getArtifact()->beforeWrite(_build, getCommand(), out_ref_id);

    // Finish the syscall and resume
    finishSyscall([=](long rc) {
      resume();

      // If the call succeeds, record the read and write
      if (rc >= 0) {
        in_ref->getArtifact()->afterRead(_build, getCommand(), in_ref_id);
        out_ref->getArtifact()->afterWrite(_build, getCommand(), out_ref_id);
      }
    });
  } else {
    // No matching FD. Just make sure the syscall fails.
    finishSyscall([=](long rc) {
      resume();

      ASSERT(rc < 0) << "Tee of unknown fd " << fd_in << " succeeded in " << this;
    });
  }
}

/************************ Directory Operations ************************/

void Thread::_mkdirat(at_fd dfd, fs::path pathname, mode_flags mode) noexcept {
  LOGF(trace, "{}: mkdirat({}={}, {}, {})", this, dfd, getPath(dfd), pathname, mode);

  // Strip a trailing slash from the pathname if it has one
  if (pathname.filename().empty()) pathname = pathname.parent_path();

  auto parent_path = pathname.parent_path();
  auto entry = pathname.filename();

  // Make a reference to the parent directory where the new directory will be added
  auto parent_ref = makePathRef(parent_path, WriteAccess, dfd);

  // Make a reference to the new directory entry that will be created
  auto entry_ref = makePathRef(pathname, NoAccess, dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Write access to the parent directory must succeed
      _build.expectResult(getCommand(), Scenario::Build, parent_ref, SUCCESS);

      // The entry must not exist prior to this call
      _build.expectResult(getCommand(), Scenario::Build, entry_ref, ENOENT);

      // Make a directory reference to get a new artifact
      auto mask = getProcess()->getUmask();
      auto dir_ref = getCommand()->nextRef();
      _build.dirRef(getCommand(), mode.getMode() & ~mask, dir_ref);

      // Link the directory into the parent dir
      _build.addEntry(getCommand(), parent_ref, entry, dir_ref);

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      _build.expectResult(getCommand(), Scenario::Build, parent_ref,
                          getCommand()->getRef(parent_ref)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, entry_ref,
                          getCommand()->getRef(entry_ref)->getResultCode());
    }
  });
}

void Thread::_renameat2(at_fd old_dfd,
                        fs::path old_path,
                        at_fd new_dfd,
                        fs::path new_path,
                        rename_flags flags) noexcept {
  LOGF(trace, "{}: renameat({}={}, {}, {}={}, {}, {})", this, old_dfd, getPath(old_dfd), old_path,
       new_dfd, getPath(new_dfd), new_path, flags);

  // Strip a trailing slash from the old path if it has one
  if (old_path.filename().empty()) old_path = old_path.parent_path();

  // Strip a trailing slash from the new path if it has one
  if (new_path.filename().empty()) new_path = new_path.parent_path();

  // Break the path to the existing file into directory and entry parts
  auto old_dir = old_path.parent_path();
  auto old_entry = old_path.filename();

  // Make references to the old directory and entry
  auto old_dir_ref = makePathRef(old_dir, WriteAccess, old_dfd);

  auto old_entry_ref = makePathRef(old_path, NoFollowAccess, old_dfd);

  // Break the path to the new file into directory and entry parts
  auto new_dir = new_path.parent_path();
  auto new_entry = new_path.filename();

  // Make a reference to the new directory
  auto new_dir_ref = makePathRef(new_dir, WriteAccess, new_dfd);

  // Make a reference to the new entry
  auto new_entry_ref = makePathRef(new_path, NoFollowAccess, new_dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Do the old and new entries refer to the same artifact?
      if (getCommand()->getRef(old_entry_ref)->getArtifact() ==
          getCommand()->getRef(new_entry_ref)->getArtifact()) {
        // Yes. The rename() call is finished, but the command depends on these two references
        // reaching the same artifact.
        _build.compareRefs(getCommand(), old_entry_ref, new_entry_ref, RefComparison::SameInstance);
        return;

      } else {
        // No. The rename() call proceeds, but the command depends on these two references
        // reaching different artifacts.
        _build.compareRefs(getCommand(), old_entry_ref, new_entry_ref,
                           RefComparison::DifferentInstances);
      }

      // The accesses to the old directory and entry must have succeeded
      _build.expectResult(getCommand(), Scenario::Build, old_dir_ref, SUCCESS);
      _build.expectResult(getCommand(), Scenario::Build, old_entry_ref, SUCCESS);

      // Unlink the old entry
      _build.removeEntry(getCommand(), old_dir_ref, old_entry, old_entry_ref);

      // The access to the new directory must also have succeeded
      _build.expectResult(getCommand(), Scenario::Build, new_dir_ref, SUCCESS);

      // Is this an exchange or noreplace option?
      if (flags.exchange()) {
        // This is an exchange, so the new_entry_ref must exist
        _build.expectResult(getCommand(), Scenario::Build, new_entry_ref, SUCCESS);

        // Unlink the new entry
        _build.removeEntry(getCommand(), new_dir_ref, new_entry, new_entry_ref);

      } else if (flags.noreplace()) {
        // This is a noreplace rename, so new_entry_ref must not exist
        _build.expectResult(getCommand(), Scenario::Build, new_entry_ref, ENOENT);
      }

      // Link into the new entry
      _build.addEntry(getCommand(), new_dir_ref, new_entry, old_entry_ref);

      // If this is an exchange, we also have to perform the swapped link
      if (flags.exchange()) {
        _build.addEntry(getCommand(), old_dir_ref, old_entry, new_entry_ref);
      }
    } else {
      // The syscall failed. Be conservative and save the result of all references. If any of them
      // change, that COULD change the syscall outcome.
      _build.expectResult(getCommand(), Scenario::Build, old_dir_ref,
                          getCommand()->getRef(old_dir_ref)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, old_entry_ref,
                          getCommand()->getRef(old_entry_ref)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, new_dir_ref,
                          getCommand()->getRef(new_dir_ref)->getResultCode());
      if (new_entry_ref) {
        _build.expectResult(getCommand(), Scenario::Build, new_entry_ref,
                            getCommand()->getRef(new_entry_ref)->getResultCode());
      }
    }
  });
}

void Thread::_getdents(int fd) noexcept {
  LOGF(trace, "{}: getdents({})", this, fd);

  // Get a reference to the artifact being read
  auto ref_id = _process->getFD(fd);
  const auto& ref = getCommand()->getRef(ref_id);

  ref->getArtifact()->beforeRead(_build, getCommand(), ref_id);

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Create a dependency on the artifact's directory list
      ref->getArtifact()->afterRead(_build, getCommand(), ref_id);
    }
  });
}

/************************ Link and Symlink Operations ************************/

void Thread::_linkat(at_fd old_dfd,
                     fs::path oldpath,
                     at_fd new_dfd,
                     fs::path newpath,
                     at_flags flags) noexcept {
  LOGF(trace, "{}: linkat({}={}, {}, {}={}, {}, {})", this, old_dfd, getPath(old_dfd), oldpath,
       new_dfd, getPath(new_dfd), newpath, flags);

  // Strip a trailing slash from the new path if it has one
  if (newpath.filename().empty()) newpath = newpath.parent_path();

  WARN_IF(flags.empty_path()) << "linkat() with AT_EMPTY_PATH is not supported yet";

  // The newpath string is the path to the new link. Split that into the directory and entry.
  auto dir_path = newpath.parent_path();
  auto entry = newpath.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makePathRef(dir_path, WriteAccess, new_dfd);

  // Get a reference to the link we are creating
  auto entry_ref = makePathRef(newpath, NoAccess, new_dfd);

  // Get a reference to the artifact we are linking into the directory
  AccessFlags target_flags = {.nofollow = true};
  if (flags.symlink_nofollow()) target_flags.nofollow = false;

  auto target_ref = makePathRef(oldpath, target_flags, old_dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc == 0) {
      // Write access to the directory must succeed
      _build.expectResult(getCommand(), Scenario::Build, dir_ref, SUCCESS);

      // The link must not exist prior to this call
      _build.expectResult(getCommand(), Scenario::Build, entry_ref, ENOENT);

      // The reference to the link target must succeed
      _build.expectResult(getCommand(), Scenario::Build, target_ref, SUCCESS);

      // Record the link operation
      _build.addEntry(getCommand(), dir_ref, entry, target_ref);

    } else {
      // The failure could be caused by the dir_ref, entry_ref, or target_ref. To be safe, just
      // record the result of resolving each of them.
      _build.expectResult(getCommand(), Scenario::Build, dir_ref,
                          getCommand()->getRef(dir_ref)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, entry_ref,
                          getCommand()->getRef(entry_ref)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, target_ref,
                          getCommand()->getRef(target_ref)->getResultCode());
    }
  });
}

void Thread::_symlinkat(fs::path target, at_fd dfd, fs::path newpath) noexcept {
  LOGF(trace, "{}: symlinkat({}, {}={}, {})", this, target, dfd, getPath(dfd), newpath);

  // Strip a trailing slash from newpath if it has one
  if (newpath.filename().empty()) newpath = newpath.parent_path();

  // The newpath string is the path to the new link. Split that into the directory and entry.
  auto dir_path = newpath.parent_path();
  auto entry = newpath.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref = makePathRef(dir_path, WriteAccess, dfd);

  // Get a reference to the link we are creating
  auto entry_ref = makePathRef(newpath, NoAccess, dfd);

  finishSyscall([=](long rc) {
    resume();

    // Did the syscall succeed?
    if (rc == 0) {
      // Write access to the directory must succeed
      _build.expectResult(getCommand(), Scenario::Build, dir_ref, SUCCESS);

      // The link must not exist prior to this call
      _build.expectResult(getCommand(), Scenario::Build, entry_ref, ENOENT);

      // Make a symlink reference to get a new artifact
      auto symlink_ref = getCommand()->nextRef();
      _build.symlinkRef(getCommand(), target, symlink_ref);

      // Link the symlink into the directory
      _build.addEntry(getCommand(), dir_ref, entry, symlink_ref);

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      _build.expectResult(getCommand(), Scenario::Build, dir_ref,
                          getCommand()->getRef(dir_ref)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, entry_ref,
                          getCommand()->getRef(entry_ref)->getResultCode());
    }
  });
}

void Thread::_readlinkat(at_fd dfd, fs::path pathname) noexcept {
  LOGF(trace, "{}: readlinkat({}={}, {})", this, dfd, getPath(dfd), pathname);

  // We need a better way to blacklist /proc/self tracking, but this is enough to make the self
  // build work
  if (pathname.string().find("/proc/self") != string::npos) {
    resume();
    return;
  }

  // We're making a reference to a symlink, so don't follow links
  auto ref_id = makePathRef(pathname, SymlinkAccess + NoFollowAccess, dfd);
  const auto& ref = getCommand()->getRef(ref_id);

  // If the reference resolves, record a pre-read dependency
  if (ref->isResolved()) {
    ref->getArtifact()->beforeRead(_build, getCommand(), ref_id);
  }

  // Finish the syscall and then resume the process
  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc >= 0) {
      // Yes. Record the successful reference
      _build.expectResult(getCommand(), Scenario::Build, ref_id, SUCCESS);

      ASSERT(ref->isResolved()) << "Failed to get artifact for successfully-read link";

      // We depend on this artifact's contents now
      ref->getArtifact()->afterRead(_build, getCommand(), ref_id);

    } else {
      // No. Record the failure
      _build.expectResult(getCommand(), Scenario::Build, ref_id, -rc);
    }
  });
}

void Thread::_unlinkat(at_fd dfd, fs::path pathname, at_flags flags) noexcept {
  LOGF(trace, "{}: unlinkat({}={}, {}, {})", this, dfd, getPath(dfd), pathname, flags);

  // Strip a trailing slash from pathname if it has one
  if (pathname.filename().empty()) pathname = pathname.parent_path();

  // Split the pathname into the parent and entry
  auto dir_path = pathname.parent_path();
  auto entry = pathname.filename();

  // Get a reference to the directory, which we will be writing
  auto dir_ref_id = makePathRef(dir_path, WriteAccess, dfd);

  // Get a reference to the entry itself
  auto ref_flags = NoFollowAccess + (flags.removedir() ? DirAccess : NotDirAccess);
  auto entry_ref_id = makePathRef(pathname, ref_flags, dfd);
  const auto& entry_ref = getCommand()->getRef(entry_ref_id);

  // If this call is removing a directory, depend on the directory contents
  if (entry_ref->isResolved() && flags.removedir()) {
    entry_ref->getArtifact()->beforeRead(_build, getCommand(), entry_ref_id);
    entry_ref->getArtifact()->afterRead(_build, getCommand(), entry_ref_id);
  }

  finishSyscall([=](long rc) {
    resume();

    // Did the call succeed?
    if (rc == 0) {
      // Both references must have succeeded
      _build.expectResult(getCommand(), Scenario::Build, dir_ref_id, SUCCESS);
      _build.expectResult(getCommand(), Scenario::Build, entry_ref_id, SUCCESS);

      // Perform the unlink
      _build.removeEntry(getCommand(), dir_ref_id, entry, entry_ref_id);

    } else {
      // The failure could be caused by either references. Record the outcome of both.
      _build.expectResult(getCommand(), Scenario::Build, dir_ref_id,
                          getCommand()->getRef(dir_ref_id)->getResultCode());
      _build.expectResult(getCommand(), Scenario::Build, entry_ref_id,
                          getCommand()->getRef(entry_ref_id)->getResultCode());
    }
  });
}

/************************ Socket Operations ************************/

void Thread::_socket(int domain, int type, int protocol) noexcept {
  WARN << "socket(2) not yet implemented. Emulating as an anonymous file.";

  finishSyscall([=](long rc) {
    resume();

    if (rc >= 0) {
      auto ref = getCommand()->nextRef();
      _build.fileRef(getCommand(), 0600, ref);
      bool cloexec = (type & SOCK_CLOEXEC) == SOCK_CLOEXEC;
      _process->addFD(_build, rc, ref, cloexec);
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
        auto ref = getCommand()->nextRef();
        _build.fileRef(getCommand(), 0600, ref);

        // Add the file descriptors
        _process->addFD(_build, sock1_fd, ref, cloexec);
        _process->addFD(_build, sock2_fd, ref, cloexec);
      }
    });
  } else {
    FAIL << "socketpair(2) for non-UNIX sockets is not implemented.";
  }
}

/************************ Process State Operations ************************/

void Thread::_umask(mode_t mask) noexcept {
  LOGF(trace, "{}: umask({:o})", this, mask);
  resume();

  getProcess()->setUmask(mask);
}

void Thread::_chdir(fs::path filename) noexcept {
  LOGF(trace, "{}: chdir({})", this, filename);

  auto ref = makePathRef(filename, ExecAccess);

  finishSyscall([=](long rc) {
    resume();

    _build.expectResult(getCommand(), Scenario::Build, ref, -rc);

    // Update the current working directory if the chdir call succeeded
    if (rc == 0) {
      _process->setWorkingDir(_build, ref);
    }
  });
}

void Thread::_chroot(fs::path filename) noexcept {
  LOGF(trace, "{}: chroot({})", this, filename);
  FAIL << "Builds that use chroot are not supported.";
}

void Thread::_pivot_root(fs::path new_root, fs::path put_old) noexcept {
  LOGF(trace, "{}: pivot_root({}, {})", this, new_root, put_old);
  FAIL << "Builds that use pivot_root are not supported.";
}

void Thread::_fchdir(int fd) noexcept {
  LOGF(trace, "{}: fchdir({})", this, fd);

  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Update the working directory
      _process->setWorkingDir(_build, _process->getFD(fd));
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

void Thread::_execveat(at_fd dfd, fs::path filename, vector<string> args) noexcept {
  LOGF(trace, "{}: execveat({}={}, {}, [\"{}\"])", this, dfd, getPath(dfd), filename,
       fmt::join(args, "\", \""));

  // The parent command needs execute access to the exec-ed path
  auto exe_ref_id = makePathRef(filename, ExecAccess, dfd);

  // Will the exec call succeed?
  if (getCommand()->getRef(exe_ref_id)->isResolved()) {
    // The reference resolved successfully, so the exec should succeed
    _build.expectResult(getCommand(), Scenario::Build, exe_ref_id, SUCCESS);
    const auto& child = _process->exec(_build, exe_ref_id, args);

    // Does the child command need to run?
    if (child->mustRun()) {
      // Yes. Run the actual exec syscall
      finishSyscall([=](long rc) {
        resume();

        // Not sure why, but exec returns -38 on success. Make sure that's what we get.
        ASSERT(rc == -38) << "Outcome of exec call did not match expected behavior.";

        // The child command depends on the contents of its executable. First, we need to know
        // what the actual executable is. Read /proc/<pid>/exe to find it
        auto real_exe_path = readlink("/proc/" + std::to_string(_process->getID()) + "/exe");

        // Now make the reference and expect success
        auto child_exe_ref_id = makePathRef(real_exe_path, ReadAccess);
        const auto& child_exe_ref = getCommand()->getRef(child_exe_ref_id);
        _build.expectResult(getCommand(), Scenario::Build, child_exe_ref_id, SUCCESS);

        ASSERT(child_exe_ref->isResolved()) << "Failed to locate artifact for executable file";

        // The child command depends on the contents of the executable
        child_exe_ref->getArtifact()->beforeRead(_build, getCommand(), child_exe_ref_id);
        child_exe_ref->getArtifact()->afterRead(_build, getCommand(), child_exe_ref_id);
      });
    } else {
      // No, the child does not need to run the exec syscall.
      // Leave the process in a stalled state so it can be exited later
      getProcess()->waitForExit([=](int exit_code) { forceExit(exit_code); });

      // Ask the build to process any deferred steps now that the child command is launched
      _build.runDeferredSteps();
    }

  } else {
    // The reference does not resolve successfully, so exec will fail
    // finishSyscall([=](long rc) {
    resume();

    //  ASSERT(getCommand()->getRef(exe_ref_id)->getResultCode() == -rc)
    //      << "Outcome of exec call did not match expected behavior.";

    _build.expectResult(getCommand(), Scenario::Build, exe_ref_id,
                        getCommand()->getRef(exe_ref_id)->getResultCode());
    //});
  }
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
      if (WIFEXITED(status)) {
        _build.join(getCommand(), exited->getCommand(), WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        // TODO: Should we encode termination by signal in some other way?
        // (yes, "some other way")
        _build.join(getCommand(), exited->getCommand(), WEXITSTATUS(status));
      }
    }
  });
}

void Thread::_waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept {
  LOGF(trace, "{}: waitid(...)", this);
  FAIL << "waitid syscall is not handled yet";
  resume();
}
