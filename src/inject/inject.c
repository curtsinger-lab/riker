#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "tracing/inject.h"

#include <dlfcn.h>
#include <emmintrin.h>
#include <errno.h>
#include <sched.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <syscall.h>
#include <unistd.h>

// How many times should a thread spin on a contended lock before backing off?
#define SPIN_BACKOFF_COUNT 512

// These symbols are provided by the assembly implementation of the safe syscall function
extern void safe_syscall_start;
extern void safe_syscall_end;

// Create a function pointer for safe, untraced system calls. Initially this will just call the
// usual traced system call wrapper
static long (*safe_syscall)(long nr, ...) = syscall;

// Has the injected library been initialized?
static bool initialized = false;

// The shared tracing channel
static struct shared_tracing_data* shmem = NULL;

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
static int fast_getdents(unsigned int fd, void* dirp, unsigned int count);

typedef struct jump {
  volatile uint16_t farjmp;
  volatile uint32_t offset;
  volatile uint64_t addr;
} __attribute__((__packed__)) jump_t;

typedef int (*main_t)(int, char**, char**);
typedef int (*start_main_t)(main_t, int, char**, void (*)(), void (*)(), void (*)(), void*);

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
  rkr_inject_init();
  // Call the real libc_start_main, but pass in the wrapped main function
  start_main_t real_libc_start_main = dlsym(RTLD_NEXT, "__libc_start_main");
  return real_libc_start_main(main_fn, argc, argv, init, fini, rtld_fini, stack_end);
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
  rc = safe_syscall(__NR_mmap, NULL, sizeof(struct shared_tracing_data), PROT_READ | PROT_WRITE,
                    MAP_SHARED, TRACING_CHANNEL_FD, 0LLU);

  // Make sure the mmap succeeded
  if (rc < 0) {
    fprintf(stderr, "WARNING: failed to map shared tracing channel.\n");
    return;
  }

  // Set the global tracing data pointer
  shmem = (struct shared_tracing_data*)rc;

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
  rkr_detour("getdents", fast_getdents);
  rkr_detour("getdents64", fast_getdents);
}

size_t channel_acquire(pid_t tid) {
  // Block until we know there's an available channel
  while (sem_wait(&shmem->available) == -1) {
  }

  // Loop until we find a channel to claim
  size_t i = tid % TRACING_CHANNEL_COUNT;
  while (true) {
    // Peek at the state of the channel
    uint8_t state = __atomic_load_n(&shmem->channels[i].state, __ATOMIC_RELAXED);

    // If the channel is available, try to acquire it.
    if (state == CHANNEL_STATE_AVAILABLE &&
        __atomic_compare_exchange_n(&shmem->channels[i].state, &state, CHANNEL_STATE_ACQUIRED, true,
                                    __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) {
      // Successfully acquired the channel
      shmem->channels[i].tid = tid;
      shmem->channels[i].buffer_pos = 0;

      return i;
    }

    i = (i + 1) % TRACING_CHANNEL_COUNT;
  }
}

void channel_release(size_t c) {
  // Reset the channel to available
  __atomic_store_n(&shmem->channels[c].state, CHANNEL_STATE_AVAILABLE, __ATOMIC_RELEASE);

  // Post an available channel
  while (sem_post(&shmem->available) == -1) {
  }
}

/// Spin until the tracer sets the channel state to PROCEED
void channel_wait(size_t c) {
  for (size_t i = 0; i < SPIN_BACKOFF_COUNT; i++) {
    // Load the channel state
    uint8_t state = __atomic_load_n(&shmem->channels[c].state, __ATOMIC_ACQUIRE);

    // Can we proceed?
    if (state == CHANNEL_STATE_PROCEED) {
      // Yes. Break out of the loop
      break;
    } else {
      _mm_pause();
    }
  }

  // Wait on the semaphore
  while (sem_wait(&shmem->channels[c].wake_tracee) != 0) {
  }
}

/// Block until the tracer allows the given syscall to proceed
void channel_enter(size_t c,
                   long syscall_nr,
                   uint64_t arg1,
                   uint64_t arg2,
                   uint64_t arg3,
                   uint64_t arg4,
                   uint64_t arg5,
                   uint64_t arg6) {
  // Fill in the "registers" to be used for tracing
  shmem->channels[c].regs.SYSCALL_NUMBER = syscall_nr;
  shmem->channels[c].regs.SYSCALL_ARG1 = arg1;
  shmem->channels[c].regs.SYSCALL_ARG2 = arg2;
  shmem->channels[c].regs.SYSCALL_ARG3 = arg3;
  shmem->channels[c].regs.SYSCALL_ARG4 = arg4;
  shmem->channels[c].regs.SYSCALL_ARG5 = arg5;
  shmem->channels[c].regs.SYSCALL_ARG6 = arg6;

  // Set the channel to a waiting-on-entry state
  __atomic_store_n(&shmem->channels[c].state, CHANNEL_STATE_PRE_SYSCALL_WAIT, __ATOMIC_RELEASE);

  // Wait
  channel_wait(c);
}

long channel_proceed(size_t c,
                     long syscall_nr,
                     uint64_t arg1,
                     uint64_t arg2,
                     uint64_t arg3,
                     uint64_t arg4,
                     uint64_t arg5,
                     uint64_t arg6,
                     bool unblock_channel) {
  uint8_t action = __atomic_load_n(&shmem->channels[c].action, __ATOMIC_ACQUIRE);
  long rc;

  if (action == CHANNEL_ACTION_CONTINUE) {
    // Release the channel and run the system call without further interruption
    channel_release(c);
    rc = safe_syscall(syscall_nr, arg1, arg2, arg3, arg4, arg5, arg6);

  } else if (action == CHANNEL_ACTION_NOTIFY) {
    // Unblock the channel if requested
    pid_t tid;
    if (unblock_channel) {
      tid = shmem->channels[c].tid;
      channel_release(c);
    }

    // Run the syscall, report the result, and move on (the tracer will release the channel)
    rc = safe_syscall(syscall_nr, arg1, arg2, arg3, arg4, arg5, arg6);

    // Acquire a new channel if it was previously unblocked
    if (unblock_channel) {
      c = channel_acquire(tid);
    }

    // Store the result of the system call in the channel
    shmem->channels[c].regs.SYSCALL_RETURN = rc;

    // Mark the channel to notify the tracer of the result
    __atomic_store_n(&shmem->channels[c].state, CHANNEL_STATE_POST_SYSCALL_NOTIFY,
                     __ATOMIC_RELEASE);

    // We do not free the channel here. The tracer will do that after seeing the syscall result.

  } else if (action == CHANNEL_ACTION_FINISH) {
    // Unblock the channel if requested
    pid_t tid;
    if (unblock_channel) {
      tid = shmem->channels[c].tid;
      channel_release(c);
    }

    // Run the syscall, report the result, and move on (the tracer will release the channel)
    rc = safe_syscall(syscall_nr, arg1, arg2, arg3, arg4, arg5, arg6);

    // Acquire a new channel if it was previously unblocked
    if (unblock_channel) {
      c = channel_acquire(tid);
    }

    // Store the result of the system call in the channel
    shmem->channels[c].regs.SYSCALL_RETURN = rc;

    // Tell the tracer that we're waiting here
    __atomic_store_n(&shmem->channels[c].state, CHANNEL_STATE_POST_SYSCALL_WAIT, __ATOMIC_RELEASE);

    // Spin until the tracer allows us to proceed
    channel_wait(c);

    // Release the channel
    channel_release(c);

  } else if (action == CHANNEL_ACTION_EXIT) {
    // Pull the exit status out of the channel registers, release it, and then exit
    uint64_t exit_status = shmem->channels[c].regs.SYSCALL_ARG1;
    channel_release(c);
    safe_syscall(__NR_exit, exit_status);

    // Should be unreachable
    abort();

  } else if (action == CHANNEL_ACTION_SKIP) {
    // Pull the syscall result out of the channel registers and release it
    rc = shmem->channels[c].regs.SYSCALL_RETURN;
    channel_release(c);

  } else {
    abort();
  }

  // Set errno if required
  if (rc < 0) {
    errno = -rc;
    return -1;
  }

  return rc;
}

uint64_t channel_buffer_string(size_t c, const char* str) {
  // If the string is null just return null
  if (str == NULL) return (uint64_t)NULL;

  // Get the size of the string (including the null terminator)
  size_t size = strlen(str) + 1;

  // Get the current position in the buffer
  size_t pos = shmem->channels[c].buffer_pos;

  // Will the string fit in the buffer?
  if (pos + size <= TRACING_CHANNEL_BUFFER_SIZE) {
    // Yes. Copy the string into the buffer
    void* dest = shmem->channels[c].buffer + pos;
    memcpy(dest, str, size);

    // Move the buffer position forward
    shmem->channels[c].buffer_pos += size;

    // Return the special tracing channel buffer pointer
    return TRACING_CHANNEL_BUFFER_PTR + pos;
  }

  // The string won't fit. Just return the pointer as-is but cast to a uint64_t
  return (uint64_t)str;
}

uint64_t channel_buffer_argv(size_t c, char* const* argv) {
  if (argv == NULL) return (uint64_t)NULL;

  // How many values are in the argv array?
  int count = 0;
  while (argv[count] != NULL) {
    count++;
  }

  // Calculate the size of the argv array including the NULL terminator
  size_t size = sizeof(char* const*) * (count + 1);

  // Get the current position int he buffer
  size_t pos = shmem->channels[c].buffer_pos;

  // Will the array fit in the buffer?
  if (pos + size <= TRACING_CHANNEL_BUFFER_SIZE) {
    // Yes. Copy the array into the buffer
    void* dest = shmem->channels[c].buffer + pos;
    memcpy(dest, argv, size);

    // Move the buffer position forward
    shmem->channels[c].buffer_pos += size;

    // Now try to move some strings into the buffer
    uint64_t* buffered_argv = (uint64_t*)dest;
    for (size_t i = 0; i < count; i++) {
      buffered_argv[i] = channel_buffer_string(c, argv[i]);
    }
  }

  // The array won't fit. Just return the existing pointer.
  return (uint64_t)argv;
}

int fast_open(const char* pathname, int flags, mode_t mode) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a syscall
  channel_enter(c, __NR_open, pathname_arg, flags, mode, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_open, (uint64_t)pathname, flags, mode, 0, 0, 0, false);
}

int fast_openat(int dfd, const char* pathname, int flags, mode_t mode) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a syscall
  channel_enter(c, __NR_openat, dfd, pathname_arg, (uint64_t)flags, (uint64_t)mode, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_openat, dfd, (uint64_t)pathname, flags, mode, 0, 0, false);
}

int fast_close(int fd) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a syscall
  channel_enter(c, __NR_close, fd, 0, 0, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_close, fd, 0, 0, 0, 0, 0, false);
}

void* fast_mmap(void* addr, size_t length, int prot, int flags, int fd, off_t offset) {
  if (fd == -1) {
    intptr_t rc = safe_syscall(__NR_mmap, (uint64_t)addr, length, prot, flags, fd, offset);
    if (rc < 0) {
      errno = -rc;
      return MAP_FAILED;
    }
    return (void*)rc;
  }

  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_mmap, (uint64_t)addr, length, prot, flags, fd, offset);

  // Finish the system call and return
  return (void*)channel_proceed(c, __NR_mmap, (uint64_t)addr, length, prot, flags, fd, offset,
                                false);
}

int fast_xstat(int ver, const char* pathname, struct stat* statbuf) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_stat, pathname_arg, (uint64_t)statbuf, 0, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_stat, (uint64_t)pathname, (uint64_t)statbuf, 0, 0, 0, 0, false);
}

int fast_lxstat(int ver, const char* pathname, struct stat* statbuf) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_lstat, pathname_arg, (uint64_t)statbuf, 0, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_lstat, (uint64_t)pathname, (uint64_t)statbuf, 0, 0, 0, 0, false);
}

int fast_fxstat(int ver, int fd, struct stat* statbuf) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_fstat, fd, (uint64_t)statbuf, 0, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_fstat, fd, (uint64_t)statbuf, 0, 0, 0, 0, false);
}

int fast_fxstatat(int ver, int dfd, const char* pathname, struct stat* statbuf, int flags) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_newfstatat, dfd, pathname_arg, (uint64_t)statbuf, flags, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_newfstatat, dfd, (uint64_t)pathname, (uint64_t)statbuf, flags, 0,
                         0, false);
}

ssize_t fast_readlink(const char* pathname, char* buf, size_t bufsiz) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_readlink, pathname_arg, (uint64_t)buf, bufsiz, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_readlink, (uint64_t)pathname, (uint64_t)buf, bufsiz, 0, 0, 0,
                         false);
}

int fast_access(const char* pathname, int mode) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname argument in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_access, pathname_arg, mode, 0, 0, 0, 0);

  // Finish the system call and return
  return channel_proceed(c, __NR_access, (uint64_t)pathname, mode, 0, 0, 0, 0, false);
}

long fast_read(int fd, void* data, size_t count) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_read, fd, (uint64_t)data, count, 0, 0, 0);

  // Finish the system call and return. Unblock the channel before issuing the syscall.
  return channel_proceed(c, __NR_read, fd, (uint64_t)data, count, 0, 0, 0, true);
}

ssize_t fast_pread(int fd, void* buf, size_t count, off_t offset) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_pread64, fd, (uint64_t)buf, count, offset, 0, 0);

  // Finish the system call and return. Unblock the channel before issuing the syscall.
  return channel_proceed(c, __NR_pread64, fd, (uint64_t)buf, count, offset, 0, 0, true);
}

long fast_write(int fd, const void* data, size_t count) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_write, fd, (uint64_t)data, count, 0, 0, 0);

  // Finish the system call and return. Unblock the channel before issuing the syscall.
  return channel_proceed(c, __NR_write, fd, (uint64_t)data, count, 0, 0, 0, true);
}

int fast_execve(const char* pathname, char* const* argv, char* const* envp) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Try to pass the pathname string in the channel's data buffer
  uint64_t pathname_arg = channel_buffer_string(c, pathname);

  // Try to pass the argv array in the channel's data buffer
  uint64_t argv_arg = channel_buffer_argv(c, argv);

  // Inform the tracer that this command is entering a library call
  channel_enter(c, __NR_execve, pathname_arg, argv_arg, (uint64_t)envp, 0, 0, 0);

  // Finish the system call and return. Unblock the channel before issuing the syscall.
  return channel_proceed(c, __NR_execve, (uint64_t)pathname, (uint64_t)argv, (uint64_t)envp, 0, 0,
                         0, true);
}

int fast_getdents(unsigned int fd, void* dirp, unsigned int count) {
  pid_t tid = gettid();

  // Find an available channel
  size_t c = channel_acquire(tid);

  // Inform the tracer that this command is entering a system call
  channel_enter(c, __NR_getdents64, fd, (uint64_t)dirp, count, 0, 0, 0);

  // Finish the system call and return.
  return channel_proceed(c, __NR_getdents64, fd, (uint64_t)dirp, count, 0, 0, 0, false);
}

/// Allow the parallel compiler wrapper to issue untraced execve syscalls
int execve_untraced(const char* pathname, char* const* argv, char* const* envp) {
  int rc = safe_syscall(__NR_execve, (uint64_t)pathname, (uint64_t)argv, (uint64_t)envp, 0, 0, 0);

  if (rc < 0) {
    errno = -rc;
    return -1;
  } else {
    return 0;
  }
}
