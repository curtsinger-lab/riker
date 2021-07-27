#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "tracing/inject.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <syscall.h>
#include <unistd.h>

// These symbols are provided by the assembly implementation of the safe syscall function
extern void safe_syscall_start;
extern void safe_syscall_end;

// Create a function pointer for safe, untraced system calls. Initially this will just call the
// usual traced system call wrapper
static long (*safe_syscall)(long nr, ...) = syscall;

// Has the injected library been initialized?
static bool initialized = false;

// The shared tracing channel
static tracing_channel_t* channel = NULL;

// The function to initialize the injected library
void rkr_inject_init();

// Replacement implementations of simple functions that use fast shared-memory tracing
static int fast_open(const char* pathname, int flags, mode_t mode);
static int fast_openat(int dfd, const char* pathname, int flags, mode_t mode);
static int fast_close(int fd);
static void* fast_mmap(void* addr, size_t length, int prot, int flags, int fd, off_t offset);
static long fast_read(int fd, void* data, size_t count);
static long fast_pread(int fd, void* data, size_t count, off_t offset);
static long fast_write(int fd, const void* data, size_t count);
static ssize_t fast_readlink(const char* pathname, char* buf, size_t bufsiz);
static int fast_access(const char* pathname, int mode);
static int fast_xstat(int ver, const char* pathname, struct stat* statbuf);
static int fast_lxstat(int ver, const char* pathname, struct stat* statbuf);
static int fast_fxstat(int ver, int fd, struct stat* statbuf);
static int fast_fxstatat(int ver, int dfd, const char* pathname, struct stat* statbuf, int flags);
static int fast_execve(const char* pathname, char* const* argv, char* const* envp);

typedef struct jump {
  volatile uint16_t farjmp;
  volatile uint32_t offset;
  volatile uint64_t addr;
} __attribute__((__packed__)) jump_t;

typedef int (*main_t)(int, char**, char**);
typedef int (*start_main_t)(main_t, int, char**, void (*)(), void (*)(), void (*)(), void*);

// The program's real main function
main_t real_main;

// The replacement main function that runs before the program's actual main
int wrapped_main(int argc, char** argv, char** envp) {
  rkr_inject_init();
  return real_main(argc, argv, envp);
}

// Interpose on __libc_start_main, and send calls to wrapped_libc_start_main
int __libc_start_main(main_t, int, char**, void (*)(), void (*)(), void (*)(), void*)
    __attribute__((weak, alias("wrapped_libc_start_main")));

// Called on startup
int wrapped_libc_start_main(main_t main_fn,
                            int argc,
                            char** argv,
                            void (*init)(),
                            void (*fini)(),
                            void (*rtld_fini)(),
                            void* stack_end) {
  // Call the real libc_start_main, but pass in the wrapped main function
  start_main_t real_libc_start_main = dlsym(RTLD_NEXT, "__libc_start_main");
  real_main = main_fn;
  return real_libc_start_main(wrapped_main, argc, argv, init, fini, rtld_fini, stack_end);
}

// Detour a function in libc to a given function address
void rkr_detour(const char* name, void* dest) {
  void* fn = dlsym(RTLD_NEXT, name);
  if (fn != NULL) {
    uintptr_t base = (uintptr_t)fn;
    uintptr_t end = base + sizeof(jump_t);
    base -= base % 0x1000;
    size_t size = 0x1000;
    if (end > base + size) {
      size = 0x2000;
    }
    safe_syscall(__NR_mprotect, base, size, PROT_WRITE);
    jump_t* j = (jump_t*)fn;
    j->farjmp = 0x25ff;
    j->offset = 0;
    j->addr = (uint64_t)dest;
    safe_syscall(__NR_mprotect, base, size, PROT_READ | PROT_EXEC);
  } /* else {
     char buf[256];
     snprintf(buf, 256, "Symbol %s not found\n", name);
     safe_syscall(__NR_write, 2, buf, strlen(buf));
   }*/
}

// Initialize the injected library
void rkr_inject_init() {
  // Map space at a fixed address for safe system calls
  void* p = (void*)syscall(__NR_mmap, SAFE_SYSCALL_PAGE, 0x1000, PROT_READ | PROT_WRITE,
                           MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED_NOREPLACE, -1, 0);
  if (p == MAP_FAILED) {
    fprintf(stderr, "WARNING: injected library failed to map safe syscall page.\n");
    return;
  }

  // Copy over the bytes for the syscall entry code
  memcpy(p, &safe_syscall_start, (intptr_t)&safe_syscall_end - (intptr_t)&safe_syscall_start);

  // Make the syscall entry code executable
  if (mprotect(p, 0x1000, PROT_READ | PROT_EXEC)) {
    fprintf(stderr, "WARNING: failed to make safe syscall page executable.\n");
    return;
  }

  // We can now issue untraced system calls with the safe_syscall function
  safe_syscall = p;

  // Does this process have the expected tracing channel fd?
  struct stat statbuf;
  long rc = safe_syscall(__NR_fstat, TRACING_CHANNEL_FD, &statbuf);
  if (rc) {
    fprintf(stderr, "WARNING: tracee does not have the expected tracing channel fd.\n");
    return;
  }

  // Map the tracing channel shared page
  rc = safe_syscall(__NR_mmap, NULL, TRACING_CHANNEL_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
                    TRACING_CHANNEL_FD, 0LLU);

  // Make sure the mmap succeeded
  if (rc < 0) {
    fprintf(stderr, "WARNING: failed to map shared tracing channel.\n");
    return;
  }

  // Set the global tracing channel
  channel = (tracing_channel_t*)rc;

  // Mark the library as initialized
  initialized = true;

  // Detour functions to fast traced implementations
  rkr_detour("open", fast_open);
  rkr_detour("__open64_nocancel", fast_open);
  rkr_detour("openat", fast_openat);
  rkr_detour("close", fast_close);
  rkr_detour("__close_nocancel", fast_close);
  rkr_detour("mmap", fast_mmap);
  rkr_detour("read", fast_read);
  rkr_detour("__read_nocancel", fast_read);
  rkr_detour("pread", fast_pread);
  rkr_detour("write", fast_write);
  rkr_detour("__write_nocancel", fast_write);
  rkr_detour("readlink", fast_readlink);
  rkr_detour("access", fast_access);
  rkr_detour("__xstat", fast_xstat);
  rkr_detour("__lxstat", fast_lxstat);
  rkr_detour("__fxstat", fast_fxstat);
  rkr_detour("__fxstatat", fast_fxstatat);

  rkr_detour("execve", fast_execve);
}

size_t channel_acquire() {
  // Spin on the tracing channel state until we can acquire it
  static __thread size_t index = 0;
  while (true) {
    uint8_t expected = CHANNEL_STATE_AVAILABLE;
    if (__atomic_compare_exchange_n(&channel[index].state, &expected, CHANNEL_STATE_ACQUIRED, false,
                                    __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) {
      channel[index].tid = gettid();
      return index;
    }

    index++;
    if (index >= TRACING_CHANNEL_COUNT) index = 0;
  }
}

void channel_release(size_t c) {
  // Reset the channel to available
  __atomic_store_n(&channel[c].state, CHANNEL_STATE_AVAILABLE, __ATOMIC_RELEASE);
}

/// Block until the tracer allows the given syscall to proceed. Returns true if the tracee should
/// wait again after the syscall completes.
bool channel_enter(size_t c,
                   long syscall_nr,
                   uint64_t arg1,
                   uint64_t arg2,
                   uint64_t arg3,
                   uint64_t arg4,
                   uint64_t arg5,
                   uint64_t arg6) {
  // Fill in the "registers" to be used for tracing
  channel[c].regs.SYSCALL_NUMBER = syscall_nr;
  channel[c].regs.SYSCALL_ARG1 = arg1;
  channel[c].regs.SYSCALL_ARG2 = arg2;
  channel[c].regs.SYSCALL_ARG3 = arg3;
  channel[c].regs.SYSCALL_ARG4 = arg4;
  channel[c].regs.SYSCALL_ARG5 = arg5;
  channel[c].regs.SYSCALL_ARG6 = arg6;

  // The channel is in syscall entry mode
  channel[c].syscall_entry = true;

  // Clear the exit_instead flag in case it was left set by a previous use of this channel
  channel[c].exit_instead = false;

  // Begin waiting on the channel
  __atomic_store_n(&channel[c].state, CHANNEL_STATE_WAITING, __ATOMIC_RELEASE);

  // Spin until the tracer allows the tracee to proceed
  while (__atomic_load_n(&channel[c].state, __ATOMIC_ACQUIRE) != CHANNEL_STATE_PROCEED) {
  }

  // Was the tracee asked to exit instead of proceeding?
  if (channel[c].exit_instead) {
    // Grab the exit status from the registers struct
    int exit_status = channel[c].regs.SYSCALL_ARG1;

    // Release the channel
    channel_release(c);

    // Exit now
    safe_syscall(__NR_exit, exit_status);
  }

  return channel[c].stop_on_exit;
}

void channel_exit(size_t c, long syscall_nr, long rc) {
  channel[c].regs.SYSCALL_NUMBER = syscall_nr;
  channel[c].syscall_entry = false;
  channel[c].return_value = rc;

  // Begin waiting on the channel
  __atomic_store_n(&channel[c].state, CHANNEL_STATE_WAITING, __ATOMIC_RELEASE);

  // Spin until the tracer allows the tracee to proceed
  while (__atomic_load_n(&channel[c].state, __ATOMIC_ACQUIRE) != CHANNEL_STATE_PROCEED) {
  }
}

uint64_t channel_buffer_string(size_t c, const char* str) {
  // Will the string fit in the channel's data buffer?
  if (str != NULL && strlen(str) < TRACING_CHANNEL_BUFFER_SIZE) {
    // Yes. Copy the string into the buffer
    strcpy(channel[c].buffer, str);
    return TRACING_CHANNEL_BUFFER_PTR;

  } else {
    // No. Just return the pointer as-is but cast to a uint64_t
    return (uint64_t)str;
  }
}

uint64_t channel_buffer_argv(size_t c, char* const* argv) {
  int count = 0;
  while (argv[count] != NULL) {
    count++;
  }

  // Will the argv array fit in the buffer?
  if ((count + 1) * sizeof(char*) <= TRACING_CHANNEL_BUFFER_SIZE) {
    // Yes. Copy it and return the special pointer value
    memcpy(channel[c].buffer, argv, (count + 1) * sizeof(char*));
    return TRACING_CHANNEL_BUFFER_PTR;

  } else {
    // No. Just return the existing pointer.
    return (uint64_t)argv;
  }
}

int fast_open(const char* pathname, int flags, mode_t mode) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a syscall
  bool stop_on_exit =
      channel_enter(c, __NR_open, pathname_arg, (uint64_t)flags, (uint64_t)mode, 0, 0, 0);

  // If we don't have to stop on exist, release the channel
  if (!stop_on_exit) channel_release(c);

  // Issue the syscall
  int rc = safe_syscall(__NR_open, pathname, flags, mode);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a syscall
    channel_exit(c, __NR_open, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_openat(int dfd, const char* pathname, int flags, mode_t mode) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a syscall
  bool stop_on_exit =
      channel_enter(c, __NR_openat, dfd, pathname_arg, (uint64_t)flags, (uint64_t)mode, 0, 0);

  if (!stop_on_exit) channel_release(c);

  // Issue the syscall
  int rc = safe_syscall(__NR_openat, dfd, pathname, flags, mode);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a syscall
    channel_exit(c, __NR_openat, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_close(int fd) {
  // Find an available channel
  size_t c = channel_acquire();

  // Inform the tracer that this command is entering a syscall
  bool stop_on_exit = channel_enter(c, __NR_close, fd, 0, 0, 0, 0, 0);

  if (!stop_on_exit) channel_release(c);

  // Issue the syscall
  int rc = safe_syscall(__NR_close, fd);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a syscall
    channel_exit(c, __NR_close, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

void* fast_mmap(void* addr, size_t length, int prot, int flags, int fd, off_t offset) {
  // Find an available channel
  size_t c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_mmap, (uint64_t)addr, length, prot, flags, fd, offset);

  if (!stop_on_exit) channel_release(c);

  long rc = safe_syscall(__NR_mmap, addr, length, prot, flags, fd, offset);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_mmap, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return MAP_FAILED;
  } else {
    return (void*)rc;
  }
}

int fast_xstat(int ver, const char* pathname, struct stat* statbuf) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_stat, pathname_arg, (uint64_t)statbuf, 0, 0, 0, 0);

  if (!stop_on_exit) channel_release(c);

  int rc = safe_syscall(__NR_stat, pathname, statbuf);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_stat, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_lxstat(int ver, const char* pathname, struct stat* statbuf) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_lstat, pathname_arg, (uint64_t)statbuf, 0, 0, 0, 0);

  if (!stop_on_exit) channel_release(c);

  int rc = safe_syscall(__NR_lstat, pathname, statbuf);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_lstat, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_fxstat(int ver, int fd, struct stat* statbuf) {
  // Find an available channel
  size_t c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_fstat, fd, (uint64_t)statbuf, 0, 0, 0, 0);

  if (!stop_on_exit) channel_release(c);

  int rc = safe_syscall(__NR_fstat, fd, statbuf);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_fstat, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_fxstatat(int ver, int dfd, const char* pathname, struct stat* statbuf, int flags) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit =
      channel_enter(c, __NR_newfstatat, dfd, pathname_arg, (uint64_t)statbuf, flags, 0, 0);

  if (!stop_on_exit) channel_release(c);

  int rc = safe_syscall(__NR_newfstatat, dfd, pathname, statbuf, flags);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_newfstatat, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

ssize_t fast_readlink(const char* pathname, char* buf, size_t bufsiz) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_readlink, pathname_arg, (uint64_t)buf, bufsiz, 0, 0, 0);

  if (!stop_on_exit) channel_release(c);

  int rc = safe_syscall(__NR_readlink, pathname, buf, bufsiz);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_readlink, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_access(const char* pathname, int mode) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_access, pathname_arg, mode, 0, 0, 0, 0);

  if (!stop_on_exit) channel_release(c);

  int rc = safe_syscall(__NR_access, pathname, mode);

  if (stop_on_exit) {
    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_access, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

long fast_read(int fd, void* data, size_t count) {
  // Find an available channel
  size_t c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_read, fd, (uint64_t)data, count, 0, 0, 0);

  channel_release(c);

  long rc = safe_syscall(__NR_read, fd, data, count);

  if (stop_on_exit) {
    c = channel_acquire();

    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_read, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

ssize_t fast_pread(int fd, void* buf, size_t count, off_t offset) {
  // Find an available channel
  size_t c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_pread64, fd, (uint64_t)buf, count, offset, 0, 0);

  channel_release(c);

  long rc = safe_syscall(__NR_pread64, fd, buf, count, offset);

  c = channel_acquire();

  // Inform the tracer that this command is exiting a library call
  if (stop_on_exit) channel_exit(c, __NR_pread64, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

long fast_write(int fd, const void* data, size_t count) {
  // Find an available channel
  size_t c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit = channel_enter(c, __NR_write, fd, (uint64_t)data, count, 0, 0, 0);

  channel_release(c);

  long rc = safe_syscall(__NR_write, fd, data, count);

  c = channel_acquire();

  // Inform the tracer that this command is exiting a library call
  if (stop_on_exit) channel_exit(c, __NR_write, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_execve(const char* pathname, char* const* argv, char* const* envp) {
  // Find an available channel
  size_t c = channel_acquire();

  // Try to pass the argv array in the channel's data buffer
  uint64_t argv_arg = channel_buffer_argv(c, argv);

  // Inform the tracer that this command is entering a library call
  bool stop_on_exit =
      channel_enter(c, __NR_execve, (uint64_t)pathname, argv_arg, (uint64_t)envp, 0, 0, 0);

  channel_release(c);

  long rc = safe_syscall(__NR_execve, pathname, argv, envp);

  if (stop_on_exit) {
    c = channel_acquire();

    // Inform the tracer that this command is exiting a library call
    channel_exit(c, __NR_execve, rc);

    // Release the channel for use by another library call
    channel_release(c);
  }

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

/*
 * Next task is to set up a channel to communicate with rkr.
 *
 * Idea: start commands with a known high-numbered FD for a temporary file. The command can mmap
 * that file to create a shared memory channel to rkr. When entering a system call, the process
 * will acquire a lock in the channel, then write out its tid and syscall registers. It should
 * then wait on a spinlock that rkr will unlock when the process can proceed before issuing the
 * actual library call.
 *
 * After the library call, the process should set a flag to indicate that it has finished the
 * library function, and then wait on a spinlock. When rkr is ready for the process to resume
 * after any post-syscall handling it will unlock that spinlock.
 *
 * If a matching system call number was traced during the library function, rkr will write the
 * syscall instruction pointer to a special field in the shared memory channel. This library can
 * then detour that system call through the safe syscall wrapper.
 *
 * Traced processes will hold the lock associated with this type of syscall entry for the entire
 * system call, so we probably need more than one. A page full of these channels should work
 * just fine. The library just has to scan through the array to find a free entry. It could
 * start at a random index (or its TID modulo the array size) and scan forward to find the first
 * free entry. Then just reuse that entry as long as it remains free.
 *
 * Messages:
 * 1. tracee claims a channel by locking it
 * 2. tracee indicates to rkr that it is ready to enter a syscall-issuing libc function
 * 3. rkr allows tracee to proceed to libc function
 * 4. tracee indicates to rkr that it has finished libc function
 * 5. rkr allows trace to resume after libc function---may include information about a syscall
 * ip
 * 6. tracee releases a channel by unlocking it
 */
