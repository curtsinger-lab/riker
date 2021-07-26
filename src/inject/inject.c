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
    base -= base % 0x1000;
    safe_syscall(__NR_mprotect, base, 0x1000, PROT_WRITE);
    jump_t* j = (jump_t*)fn;
    j->farjmp = 0x25ff;
    j->offset = 0;
    j->addr = (uint64_t)dest;
    safe_syscall(__NR_mprotect, base, 0x1000, PROT_READ | PROT_EXEC);
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
                    TRACING_CHANNEL_FD, 0);

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
}

tracing_channel_t* channel_acquire() {
  // Spin on the tracing channel state until we can acquire it
  size_t index = 0;
  while (true) {
    tracing_channel_t* c = &channel[index];
    uint8_t expected = CHANNEL_STATE_AVAILABLE;
    if (__atomic_compare_exchange_n(&c->state, &expected, CHANNEL_STATE_ACQUIRED, false,
                                    __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) {
      return c;
    }

    index++;
    if (index >= TRACING_CHANNEL_COUNT) index = 0;
  }
}

void channel_enter(tracing_channel_t* c,
                   long syscall_nr,
                   uint64_t arg1,
                   uint64_t arg2,
                   uint64_t arg3,
                   uint64_t arg4,
                   uint64_t arg5,
                   uint64_t arg6) {
  // Fill in the "registers" to be used for tracing
  memset(&c->regs, 0, sizeof(c->regs));
  c->regs.SYSCALL_NUMBER = syscall_nr;
  c->regs.SYSCALL_ARG1 = arg1;
  c->regs.SYSCALL_ARG2 = arg2;
  c->regs.SYSCALL_ARG3 = arg3;
  c->regs.SYSCALL_ARG4 = arg4;
  c->regs.SYSCALL_ARG5 = arg5;
  c->regs.SYSCALL_ARG6 = arg6;

  // Save the traced thread's ID
  c->tid = gettid();

  // Mark the channel for entry
  __atomic_store_n(&c->state, CHANNEL_STATE_ENTRY, __ATOMIC_RELEASE);

  // Spin until the tracer allows the tracee to proceed. Either proceed message is acceptable. Riker
  // may send EXIT_PROCEED if it does not need to see the outcome of the system call.
  uint8_t state;
  do {
    state = __atomic_load_n(&c->state, __ATOMIC_ACQUIRE);
  } while (state != CHANNEL_STATE_ENTRY_PROCEED && state != CHANNEL_STATE_EXIT_PROCEED);
}

void channel_exit(tracing_channel_t* c, long rc) {
  c->return_value = rc;

  // Mark the channel as an exiting library call, but make sure it's in the ENTRY_PROCEED mode
  uint8_t expected = CHANNEL_STATE_ENTRY_PROCEED;
  if (!__atomic_compare_exchange_n(&c->state, &expected, CHANNEL_STATE_EXIT, false,
                                   __ATOMIC_RELEASE, __ATOMIC_ACQUIRE)) {
    // If the exchange failed then riker must have put the channel directly into EXIT_PROCEED mode
    if (expected == CHANNEL_STATE_EXIT_PROCEED) return;
  }

  // Otherwise just spin until the tracer allows the tracee to proceed
  while (__atomic_load_n(&c->state, __ATOMIC_ACQUIRE) != CHANNEL_STATE_EXIT_PROCEED) {
  }
}

void channel_release(tracing_channel_t* c) {
  // Reset the channel to available
  __atomic_store_n(&c->state, CHANNEL_STATE_AVAILABLE, __ATOMIC_RELEASE);
}

int fast_open(const char* pathname, int flags, mode_t mode) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (pathname != NULL && strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a syscall
  channel_enter(c, __NR_open, pathname_arg, (uint64_t)flags, (uint64_t)mode, 0, 0, 0);

  // Issue the syscall
  int rc = safe_syscall(__NR_open, pathname, flags, mode);

  // Inform the tracer that this command is exiting a syscall
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_openat(int dfd, const char* pathname, int flags, mode_t mode) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (pathname != NULL && strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a syscall
  channel_enter(c, __NR_openat, dfd, pathname_arg, (uint64_t)flags, (uint64_t)mode, 0, 0);

  // Issue the syscall
  int rc = safe_syscall(__NR_openat, dfd, pathname, flags, mode);

  // Inform the tracer that this command is exiting a syscall
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_close(int fd) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  // Inform the tracer that this command is entering a syscall
  channel_enter(c, __NR_close, fd, 0, 0, 0, 0, 0);

  // Issue the syscall
  int rc = safe_syscall(__NR_close, fd);

  // Inform the tracer that this command is exiting a syscall
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

void* fast_mmap(void* addr, size_t length, int prot, int flags, int fd, off_t offset) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_mmap, (uint64_t)addr, length, prot, flags, fd, offset);

  long rc = safe_syscall(__NR_mmap, addr, length, prot, flags, fd, offset);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return MAP_FAILED;
  } else {
    return (void*)rc;
  }
}

int fast_xstat(int ver, const char* pathname, struct stat* statbuf) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_stat, pathname_arg, (uint64_t)statbuf, 0, 0, 0, 0);

  int rc = safe_syscall(__NR_stat, pathname, statbuf);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_lxstat(int ver, const char* pathname, struct stat* statbuf) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_lstat, pathname_arg, (uint64_t)statbuf, 0, 0, 0, 0);

  int rc = safe_syscall(__NR_lstat, pathname, statbuf);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_fxstat(int ver, int fd, struct stat* statbuf) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_fstat, fd, (uint64_t)statbuf, 0, 0, 0, 0);

  int rc = safe_syscall(__NR_fstat, fd, statbuf);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_fxstatat(int ver, int dfd, const char* pathname, struct stat* statbuf, int flags) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_newfstatat, dfd, pathname_arg, (uint64_t)statbuf, flags, 0, 0);

  int rc = safe_syscall(__NR_newfstatat, dfd, pathname, statbuf, flags);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

ssize_t fast_readlink(const char* pathname, char* buf, size_t bufsiz) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_readlink, pathname_arg, (uint64_t)buf, bufsiz, 0, 0, 0);

  int rc = safe_syscall(__NR_readlink, pathname, buf, bufsiz);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

int fast_access(const char* pathname, int mode) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  uint64_t pathname_arg = (uint64_t)pathname;

  // If we the pathname will fit in the channel's buffer, put it there
  if (strlen(pathname) < TRACING_CHANNEL_BUFFER_SIZE) {
    strcpy(c->buffer, pathname);
    pathname_arg = TRACING_CHANNEL_BUFFER_PTR;
  }

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_access, pathname_arg, mode, 0, 0, 0, 0);

  int rc = safe_syscall(__NR_access, pathname, mode);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

long fast_read(int fd, void* data, size_t count) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_read, fd, (uint64_t)data, count, 0, 0, 0);

  long rc = safe_syscall(__NR_read, fd, data, count);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return rc;
  }
}

ssize_t fast_pread(int fd, void* buf, size_t count, off_t offset) {
  // Find an available channel
  tracing_channel_t* c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_pread64, fd, (uint64_t)buf, count, offset, 0, 0);

  long rc = safe_syscall(__NR_pread64, fd, buf, count, offset);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

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
  tracing_channel_t* c = channel_acquire();

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_write, fd, (uint64_t)data, count, 0, 0, 0);

  long rc = safe_syscall(__NR_write, fd, data, count);

  // Inform the tracer that this command is exiting a library call
  channel_exit(c, rc);

  // Release the channel for use by another library call
  channel_release(c);

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
