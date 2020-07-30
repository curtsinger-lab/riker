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

user_regs_struct Process::getRegisters() noexcept {
  struct user_regs_struct regs;
  FAIL_IF(ptrace(PTRACE_GETREGS, _pid, nullptr, &regs)) << "Failed to get registers: " << ERR;
  return regs;
}

void Process::resume() noexcept {
  FAIL_IF(ptrace(PTRACE_CONT, _pid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

void Process::finishSyscall(function<void(long)> handler) noexcept {
  ASSERT(!_post_syscall_handler) << "Process already has an unexecuted post-syscall handler";

  // Set the post-syscall handler
  _post_syscall_handler = handler;

  // Allow the tracee to resume until its syscall finishes
  FAIL_IF(ptrace(PTRACE_SYSCALL, _pid, nullptr, 0)) << "Failed to resume child: " << ERR;
}

void Process::syscallFinished() noexcept {
  ASSERT(_post_syscall_handler) << "Process does not have a post-syscall handler";

  // Set up an empty handler
  function<void(long)> handler;

  // Swap it with the registered handler (to clear the registered one)
  _post_syscall_handler.swap(handler);

  // Now extract the return code from the syscall

  // Clear errno so we can check for errors
  errno = 0;
  long result = ptrace(PTRACE_PEEKUSER, _pid, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
  FAIL_IF(errno != 0) << "Failed to read return value from traced process: " << ERR;

  // Run the handler
  handler(result);
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
  if (p.is_absolute()) return _build.access(_command, _root, p.relative_path(), flags);

  // Handle the special CWD file descriptor to resolve relative to cwd
  if (at == AT_FDCWD) return _build.access(_command, _cwd, p.relative_path(), flags);

  // The path is resolved relative to some file descriptor
  auto base = _fds.at(at).getRef()->as<Access>();

  ASSERT(base) << "Attempted to resolve a path relative to an anonymous reference";

  return _build.access(_command, base, p.relative_path(), flags);
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
  LOG(syscall) << _pid << ": openat(" << dfd << ", \"" << filename << "\", " << flags << ", "
               << std::oct << mode << ")";

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
        auto anon = _build.file(_command, mode);

        // Record the reference in the process' file descriptor table
        auto [iter, inserted] = _fds.emplace(fd, FileDescriptor(anon, ref->getFlags().w, cloexec));

        ASSERT(inserted) << "Newly-opened file descriptor conflicted with an existing entry";

      } else {
        // If the file is truncated by the open call, set the contents in the artifact
        if (ref->getFlags().truncate) {
          auto written = make_shared<FileVersion>(FileFingerprint::makeEmpty());
          _build.apply<FileVersion>(_command, ref, written);
        }

        // Record the reference in the correct location in this process' file descriptor table
        auto [iter, inserted] = _fds.emplace(fd, FileDescriptor(ref, ref->getFlags().w, cloexec));

        ASSERT(inserted) << "Newly-opened file descriptor conflicted with an existing entry";
      }

    } else {
      // The command observed a failed openat, so add the error predicate to the command log
      // Negate fd because syscalls return negative errors
      ref->expectResult(-fd);
    }
  });
}

void Process::_mknodat(int dfd, string filename, mode_t mode, unsigned dev) noexcept {
  LOG(syscall) << _pid << ": mknodat(" << dfd << ", \"" << filename << "\", " << std::oct << mode
               << std::dec << ", " << dev << ")";

  if ((mode & S_IFMT) == S_IFREG) {
    // Handle regular file creation with openat
    _openat(dfd, filename, O_CREAT | O_EXCL, mode);
  } else {
    // TODO: Handle named pipes?

    WARN << "Unsupported use of mknodat";
    resume();
  }
}

void Process::_close(int fd) noexcept {
  LOG(syscall) << _pid << ": close(" << fd << ")";

  // NOTE: We assume close calls always succeed. Erasing a non-existent file descriptor is
  // harmless

  // Resume the process
  resume();

  // Remove the file descriptor
  _fds.erase(fd);
}

/************************ Pipes ************************/

void Process::_pipe2(int* fds, int flags) noexcept {
  LOG(syscall) << _pid << ": pipe2(" << (void*)fds << ", " << flags << ")";

  finishSyscall([this, fds, flags](long rc) {
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
    auto ref = _build.pipe(_command);

    ASSERT(ref->isResolved()) << "Failed to get artifact for pipe";

    // Check if this pipe is closed on exec
    bool cloexec = (flags & O_CLOEXEC) == O_CLOEXEC;

    // Fill in the file descriptor entries
    auto [iter1, inserted1] = _fds.emplace(read_pipefd, FileDescriptor(ref, false, cloexec));
    auto [iter2, inserted2] = _fds.emplace(write_pipefd, FileDescriptor(ref, true, cloexec));

    ASSERT(inserted1 && inserted2) << "Pipe file descriptors conflicted with existing entries";
  });
}

/************************ File Descriptor Manipulation ************************/

void Process::_dup(int fd) noexcept {
  LOG(syscall) << _pid << ": dup(" << fd << ")";

  // Finish the syscall to get the new file descriptor, then resume the process
  finishSyscall([=](int newfd) {
    resume();

    // If the syscall failed, do nothing
    if (newfd == -1) return;

    // Add the new entry for the duped fd
    _fds[newfd] = _fds.at(fd);

    // Duped fds do not inherit the cloexec flag
    _fds.at(newfd).setCloexec(false);
  });
}

void Process::_dup3(int oldfd, int newfd, int flags) noexcept {
  LOG(syscall) << _pid << ": dup3(" << oldfd << ", " << newfd << ", " << flags << ")";

  // dup3 returns the new file descriptor, or error
  // Finish the syscall so we know what file descriptor to add to our table
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, we have nothing more to do
    // Note: this is different than a failed file access. This failure should not be affected
    //       by the state of the filesystem, so we don't have to log it.
    if (rc == -1) return;

    // Duplicate the file descriptor
    _fds[rc] = _fds.at(oldfd);

    // If the flags include O_CLOEXEC, we have to set that property on the new file descriptor
    // If O_CLOEXEC is not set, any dup-ed fd is NOT cloexec
    _fds.at(rc).setCloexec((flags & O_CLOEXEC) == O_CLOEXEC);
  });
}

void Process::_fcntl(int fd, int cmd, unsigned long arg) noexcept {
  LOG(syscall) << _pid << ": fcntl(" << fd << ", " << cmd << ", " << arg << ")";

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
    _fds.at(fd).setCloexec(arg & FD_CLOEXEC);

  } else {
    resume();
    // Some other operation we do not need to handle
    // TODO: Filter these stops out with BPF/seccomp
  }
}

/************************ Metadata Operations ************************/

void Process::_faccessat(int dirfd, string pathname, int mode, int flags) noexcept {
  LOG(syscall) << _pid << ": faccessat(" << dirfd << ", \"" << pathname << "\", " << std::oct
               << mode << std::dec << ", " << flags << ")";

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

void Process::_fstatat(int dirfd, string pathname, struct stat* statbuf, int flags) noexcept {
  LOG(syscall) << _pid << ": fstatat(" << dirfd << ", \"" << pathname << "\", " << (void*)statbuf
               << ", " << flags << ")";

  // If the AT_EMPTY_PATH flag is set, we are statting an already-opened file descriptor
  // Otherwise, this is just a normal stat call
  if ((flags & AT_EMPTY_PATH) == AT_EMPTY_PATH) {
    resume();

    // This is essentially an fstat call
    // Record the dependency on metadata
    _build.match<MetadataVersion>(_command, _fds.at(dirfd).getRef());

  } else {
    // Finish the syscall to see if the reference succeeds
    finishSyscall([=](long rc) {
      resume();

      // This is a regular stat call (with an optional base directory descriptor)
      auto ref = makeAccess(pathname, {}, dirfd);

      // Record the outcome of the syscall
      ref->expectResult(-rc);

      // Log the success or failure
      if (rc == 0) {
        ASSERT(ref->isResolved()) << "Unable to locate artifact for stat-ed file " << ref;

        // Record the dependence on the artifact's metadata
        _build.match<MetadataVersion>(_command, ref);
      }
    });
  }
}

void Process::_fchown(int fd, uid_t user, gid_t group) noexcept {
  LOG(syscall) << _pid << ": fchown(" << fd << ", " << user << ", " << group << ")";

  // Look for the descriptor. If it doesn't exist, resume the process and return
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    resume();
    return;
  }

  // Get the descriptor
  auto& descriptor = iter->second;

  // The command depends on the old metadata
  _build.match<MetadataVersion>(_command, descriptor.getRef());

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.apply<MetadataVersion>(_command, descriptor.getRef(), make_shared<MetadataVersion>());
  });
}

void Process::_fchownat(int dfd, string filename, uid_t user, gid_t group, int flags) noexcept {
  LOG(syscall) << _pid << ": fchownat(" << dfd << ", \"" << filename << "\", " << user << ", "
               << group << ", " << flags << ")";

  // Make a reference to the file that will be chown-ed.
  bool nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
  auto ref = makeAccess(filename, AccessFlags{.nofollow = nofollow}, dfd);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->isResolved()) {
    _build.match<MetadataVersion>(_command, ref);
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
      _build.apply<MetadataVersion>(_command, ref, make_shared<MetadataVersion>());

    } else {
      // No. Record the failure
      ref->expectResult(-rc);
    }
  });
}

void Process::_fchmod(int fd, mode_t mode) noexcept {
  LOG(syscall) << _pid << ": fchmod(" << fd << ", " << std::oct << mode << ")";

  // Look for the descriptor. If it doesn't exist, resume the process and return
  auto iter = _fds.find(fd);
  if (iter == _fds.end()) {
    resume();
    return;
  }

  // Get the descriptor
  auto& descriptor = iter->second;

  // The command depends on the old metadata
  _build.match<MetadataVersion>(_command, descriptor.getRef());

  // Finish the sycall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the syscall failed, there's nothing to do
    if (rc) return;

    // The command updates the metadata
    _build.apply<MetadataVersion>(_command, descriptor.getRef(), make_shared<MetadataVersion>());
  });
}

void Process::_fchmodat(int dfd, string filename, mode_t mode, int flags) noexcept {
  LOG(syscall) << _pid << ": fchmodat(" << dfd << ", \"" << filename << "\", " << std::oct << mode
               << std::dec << ", " << flags << ")";

  // Make a reference to the file that will be chmod-ed.
  bool nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
  auto ref = makeAccess(filename, AccessFlags{.nofollow = nofollow}, dfd);

  // If the artifact exists, we depend on its metadata (chmod does not replace all metadata
  // values)
  if (ref->isResolved()) {
    _build.match<MetadataVersion>(_command, ref);
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
      _build.apply<MetadataVersion>(_command, ref, make_shared<MetadataVersion>());

    } else {
      // No. Record the failure
      ref->expectResult(-rc);
    }
  });
}

/************************ File Content Operations ************************/

void Process::_read(int fd) noexcept {
  LOG(syscall) << _pid << ": read(" << fd << ")";

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // Create a dependency on the artifact's contents
    const auto& descriptor = _fds.at(fd);
    _build.match<FileVersion>(_command, descriptor.getRef());
  });
}

void Process::_write(int fd) noexcept {
  LOG(syscall) << _pid << ": write(" << fd << ")";

  // Get the descriptor
  const auto& descriptor = _fds.at(fd);

  // Record our dependency on the old contents of the artifact
  _build.match<FileVersion>(_command, descriptor.getRef());

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    // If the write syscall failed, there's no need to log a write
    if (rc == -1) return;

    // Record the update to the artifact contents
    _build.apply<FileVersion>(_command, descriptor.getRef(), make_shared<FileVersion>());
  });
}

void Process::_mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) noexcept {
  LOG(syscall) << _pid << ": mmap(" << fd << ")";

  // Skip anonymous mappings. We never need to handle these because they only allow communication
  // within a single command.
  if (fd == -1) {
    resume();
    return;
  }

  // Run the syscall to find out if the mmap succeeded
  finishSyscall([=](long rc) {
    void* result = (void*)rc;

    // If the map failed there's nothing to log
    if (result == MAP_FAILED) {
      resume();
      return;
    }

    // Get the descriptor from the fd number
    const auto& descriptor = _fds.at(fd);

    // By mmapping a file, the command implicitly depends on its contents at the time of
    // mapping.
    _build.match<FileVersion>(_command, descriptor.getRef());

    // If the mapping is writable, and the file was opened in write mode, the command
    // is also effectively setting the contents of the file.
    bool writable = (prot & PROT_WRITE) && descriptor.isWritable();
    if (writable) {
      _build.apply<FileVersion>(_command, descriptor.getRef(), make_shared<FileVersion>());
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

void Process::_truncate(string pathname, long length) noexcept {
  LOG(syscall) << _pid << ": truncate(\"" << pathname << "\", " << length << ")";

  // Make an access to the reference that will be truncated
  auto ref = makeAccess(pathname, AccessFlags{.w = true});

  // If length is non-zero, we depend on the previous contents
  // This only applies if the artifact exists
  if (length > 0 && ref->isResolved()) {
    _build.match<FileVersion>(_command, ref);
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
      _build.apply<FileVersion>(_command, ref, make_shared<FileVersion>());
    }
  });
}

void Process::_ftruncate(int fd, long length) noexcept {
  LOG(syscall) << _pid << ": ftruncate(" << fd << ", " << length << ")";

  // Get the descriptor
  const auto& descriptor = _fds.at(fd);

  // If length is non-zero, this is a write so we depend on the previous contents
  if (length > 0) {
    _build.match<FileVersion>(_command, descriptor.getRef());
  }

  // Finish the syscall and resume the process
  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Record the update to the artifact contents
      _build.apply<FileVersion>(_command, descriptor.getRef(), make_shared<FileVersion>());
    }
  });
}

void Process::_tee(int fd_in, int fd_out) noexcept {
  LOG(syscall) << _pid << ": tee(" << fd_in << ", " << fd_out << ")";

  // Get the descriptors
  const auto& in_desc = _fds.at(fd_in);
  const auto& out_desc = _fds.at(fd_out);

  // The command depends on the contents of the output file, unless it is totally overwritten (not
  // checked yet)
  _build.match<FileVersion>(_command, out_desc.getRef());

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // The command has now read the input file, so it depends on the contents there
    _build.match<FileVersion>(_command, in_desc.getRef());

    // The command has now set the contents of the output file
    _build.apply<FileVersion>(_command, out_desc.getRef(), make_shared<FileVersion>());
  });
}

/************************ Directory Operations ************************/

void Process::_mkdirat(int dfd, string pathname, mode_t mode) noexcept {
  LOG(syscall) << _pid << ": mkdirat(" << dfd << ", \"" << pathname << "\", " << std::oct << mode
               << ")";

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
      auto dir_ref = _build.dir(_command, mode);

      // Link the directory into the parent dir
      _build.apply(_command, parent_ref, make_shared<AddEntry>(entry, dir_ref));

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      parent_ref->expectResult(parent_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
    }
  });
}

void Process::_renameat2(int old_dfd,
                         string old_name,
                         int new_dfd,
                         string new_name,
                         int flags) noexcept {
  LOG(syscall) << _pid << ": renameat2(" << old_dfd << ", \"" << old_name << "\", " << new_dfd
               << ", \"" << new_name << "\", " << flags << ")";

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
      _build.apply(_command, old_dir_ref, make_shared<RemoveEntry>(old_entry, old_entry_ref));

      // The access to the new directory must also have succeeded
      new_dir_ref->expectResult(SUCCESS);

      // Is this an exchange or noreplace option?
      if (flags & RENAME_EXCHANGE) {
        // This is an exchange, so the new_entry_ref must exist
        new_entry_ref->expectResult(SUCCESS);

        // Unlink the new entry
        _build.apply(_command, new_dir_ref, make_shared<RemoveEntry>(new_entry, new_entry_ref));

      } else if (flags & RENAME_NOREPLACE) {
        // This is a noreplace rename, so new_entry_ref must not exist
        new_entry_ref->expectResult(ENOENT);
      }

      // Link into the new entry
      _build.apply(_command, new_dir_ref, make_shared<AddEntry>(new_entry, old_entry_ref));

      // If this is an exchange, we also have to perform the swapped link
      if (flags & RENAME_EXCHANGE) {
        _build.apply(_command, old_dir_ref, make_shared<AddEntry>(old_entry, new_entry_ref));
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

void Process::_getdents(int fd) noexcept {
  LOG(syscall) << _pid << ": getdents(" << fd << ")";

  // Finish the syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // Create a dependency on the artifact's directory list
    const auto& descriptor = _fds.at(fd);
    _build.match<ListedDir>(_command, descriptor.getRef());
  });
}

/************************ Link and Symlink Operations ************************/

void Process::_linkat(int old_dfd,
                      string oldpath,
                      int new_dfd,
                      string newpath,
                      int flags) noexcept {
  LOG(syscall) << _pid << ": linkat(" << old_dfd << ", \"" << oldpath << "\", " << new_dfd << ", \""
               << newpath << "\", " << flags << ")";

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
      _build.apply(_command, dir_ref, make_shared<AddEntry>(entry, target_ref));

    } else {
      // The failure could be caused by the dir_ref, entry_ref, or target_ref. To be safe, just
      // record the result of resolving each of them.
      dir_ref->expectResult(dir_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
      target_ref->expectResult(target_ref->getResolution());
    }
  });
}

void Process::_symlinkat(string target, int dfd, string newpath) noexcept {
  LOG(syscall) << _pid << ": symlinkat(\"" << target << "\", " << dfd << ", \"" << newpath << "\")";

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
      auto symlink_ref = _build.symlink(_command, target);

      // Link the symlink into the directory
      _build.apply(_command, dir_ref, make_shared<AddEntry>(entry, symlink_ref));

    } else {
      // The failure could be caused by either dir_ref or entry_ref. Record the result of both.
      dir_ref->expectResult(dir_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
    }
  });
}

void Process::_readlinkat(int dfd, string pathname) noexcept {
  LOG(syscall) << _pid << ": readlinkat(" << dfd << ", \"" << pathname << "\")";

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
      _build.match<SymlinkVersion>(_command, ref);

    } else {
      // No. Record the failure
      ref->expectResult(-rc);
    }
  });
}

void Process::_unlinkat(int dfd, string pathname, int flags) noexcept {
  LOG(syscall) << _pid << ": unlinkat(" << dfd << ", \"" << pathname << "\", " << flags << ")";

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
      _build.match<ListedDir>(_command, entry_ref);
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
      _build.apply(_command, dir_ref, make_shared<RemoveEntry>(entry, entry_ref));

    } else {
      // The failure could be caused by either references. Record the outcome of both.
      dir_ref->expectResult(dir_ref->getResolution());
      entry_ref->expectResult(entry_ref->getResolution());
    }
  });
}

/************************ Process State Operations ************************/

void Process::_chdir(string filename) noexcept {
  LOG(syscall) << _pid << ": chdir(\"" << filename << "\")";

  finishSyscall([=](long rc) {
    resume();

    // Update the current working directory if the chdir call succeeded
    if (rc == 0) {
      _cwd = makeAccess(filename, AccessFlags{.x = true});
      _cwd->expectResult(SUCCESS);
      ASSERT(_cwd->isResolved()) << "Failed to resolve current working directory";
    }
  });
}

void Process::_chroot(string filename) noexcept {
  LOG(syscall) << _pid << ": chroot(\"" << filename << "\")";
  FAIL << "Builds that use chroot are not supported.";
}

void Process::_pivot_root(string new_root, string put_old) noexcept {
  LOG(syscall) << _pid << ": pivot_root(\"" << new_root << "\", \"" << put_old << "\")";
  FAIL << "Builds that use pivot_root are not supported.";
}

void Process::_fchdir(int fd) noexcept {
  LOG(syscall) << _pid << ": fchdir(" << fd << ")";

  finishSyscall([=](long rc) {
    resume();

    if (rc == 0) {
      // Get the path to the artifact this descriptor references
      const auto& descriptor = _fds.at(fd);
      auto a = descriptor.getRef()->as<Access>();

      // Make sure there really is a path
      ASSERT(a) << "fchdir to an artifact with no path should not succeed";

      // Update the working directory
      _cwd = a;
    }
  });
}

void Process::_execveat(int dfd,
                        string filename,
                        vector<string> args,
                        vector<string> env) noexcept {
  LOG(syscall) << _pid << ": execveat(" << dfd << ", \"" << filename << "\", ...)";

  // Finish the exec syscall and resume
  finishSyscall([=](long rc) {
    resume();

    // The parent command needs execute access to the exec-ed path
    auto exe_ref = makeAccess(filename, AccessFlags{.x = true}, dfd);

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

    // The child command depends on the contents of its executable. First, we need to know what
    // the actual executable is. Read /proc/<pid>/exe to find it
    auto real_exe_path = readlink("/proc/" + std::to_string(_pid) + "/exe");

    // Now make the reference and expect success
    auto child_exe_ref = makeAccess(real_exe_path, AccessFlags{.r = true});
    child_exe_ref->expectResult(SUCCESS);

    ASSERT(child_exe_ref->isResolved()) << "Failed to locate artifact for executable file";

    // The child command depends on the contents of the executable
    _build.match<FileVersion>(_command, child_exe_ref);

    // TODO: Remove mmaps from the previous command, unless they're mapped in multiple processes
    // that participate in that command. This will require some extra bookkeeping. For now, we
    // over-approximate the set of commands that have a file mmapped.
  });
}

void Process::_wait4(pid_t pid, int* wstatus, int options) noexcept {
  LOG(syscall) << _pid << ": wait4(" << pid << ", " << (void*)wstatus << ", " << options << ")";

  finishSyscall([=](long rc) {
    int status = readData<int>((uintptr_t)wstatus);
    resume();

    // If the syscall failed or returned immediately after WNOHANG, stop processing
    if (rc <= 0) return;

    // Get the process that was returned
    auto p = _tracer.getExited(rc);

    ASSERT(p) << "wait4 syscall returned an untracked PID " << rc;

    if (p->_command != _command) {
      _build.exit(p->_command, WEXITSTATUS(status));
      if (WIFEXITED(status)) {
        _build.join(_command, p->_command, WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        // TODO: Should we encode termination by signal in some other way?
        // (yes, "some other way")
        _build.join(_command, p->_command, WEXITSTATUS(status));
      }
    }
  });
}

void Process::_waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept {
  LOG(syscall) << _pid << ": waitid(...)";
  WARN << "waitid syscall is not handled yet";
  resume();
}
