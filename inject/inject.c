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

static uint8_t safe_syscall_code[] = {
    0xf3, 0x0f, 0x1e, 0xfa,        // endbr64
    0x48, 0x89, 0xf8,              // mov %rdi, %rax
    0x48, 0x89, 0xf7,              // mov %rsi, %rdi
    0x48, 0x89, 0xd6,              // mov %rdx, %rsi
    0x48, 0x89, 0xca,              // mov %rcx, %rdx
    0x4d, 0x89, 0xc2,              // mov %r8, %r10
    0x4d, 0x89, 0xc8,              // mov %r9, %r8
    0x4c, 0x8b, 0x4c, 0x24, 0x08,  // mov 0x8(%rsp), %r9
    0x0f, 0x05,                    // syscall
    0xc3                           // retq
};

// Create a function pointer for safe, untraced system calls. Initially this will just call the
// usual traced system call wrapper
static long (*safe_syscall)(long nr, ...) = syscall;

// Pointers to real library functions wrapped in this library
static int (*real_open)(const char* pathname, int flags, mode_t mode) = NULL;

// Has the injected library been initialized?
static bool initialized = false;

// The shared tracing channel
static tracing_channel_t* channel = NULL;

__attribute__((constructor)) void init() {
  // Populate the pointers to wrapped libc functions
  real_open = dlsym(RTLD_NEXT, "open");

  // Map space at a fixed address for safe system calls
  void* p = mmap(SAFE_SYSCALL_PAGE, 0x1000, PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED_NOREPLACE, -1, 0);
  if (p == MAP_FAILED) {
    fprintf(stderr, "WARNING: injected library failed to map safe syscall page.\n");
    return;
  }

  // Copy over the bytes for the syscall entry code
  memcpy(p, safe_syscall_code, sizeof(safe_syscall_code));

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
}

tracing_channel_t* channel_acquire() {
  // Spin on the tracing channel state until we can acquire it
  uint8_t expected = CHANNEL_STATE_AVAILABLE;
  while (!__atomic_compare_exchange_n(&channel->state, &expected, CHANNEL_STATE_ACQUIRED, false,
                                      __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) {
  }

  return channel;
}

void channel_enter(tracing_channel_t* c,
                   long syscall_nr,
                   uint64_t arg1,
                   uint64_t arg2,
                   uint64_t arg3,
                   uint64_t arg4,
                   uint64_t arg5,
                   uint64_t arg6,
                   long alternate_syscall_nr) {
  // Fill in the "registers" to be used for tracing
  memset(&c->regs, 0, sizeof(c->regs));
  c->regs.SYSCALL_NUMBER = syscall_nr;
  c->regs.SYSCALL_ARG1 = arg1;
  c->regs.SYSCALL_ARG2 = arg2;
  c->regs.SYSCALL_ARG3 = arg3;
  c->regs.SYSCALL_ARG4 = arg4;
  c->regs.SYSCALL_ARG5 = arg5;
  c->regs.SYSCALL_ARG6 = arg6;

  // Allow an alternate (usually more-general) system call to be traced and skipped while handling
  // this system call via library tracing
  c->alternate_syscall = alternate_syscall_nr;

  // Save the traced thread's ID
  c->tid = safe_syscall(__NR_gettid);

  // Clear data to be written by the tracer
  c->traced_syscall_ip = 0;

  // Mark the channel for entry
  __atomic_store_n(&c->state, CHANNEL_STATE_ENTRY, __ATOMIC_RELEASE);

  // safe_syscall(__NR_write, 2, "entering...\n", 12);

  // Spin until the tracer allows the tracee to proceed
  while (__atomic_load_n(&c->state, __ATOMIC_ACQUIRE) != CHANNEL_STATE_ENTRY_PROCEED) {
  }
}

void channel_exit(tracing_channel_t* c, long rc) {
  c->return_value = rc;

  // Mark the channel as an exiting library call
  __atomic_store_n(&c->state, CHANNEL_STATE_EXIT, __ATOMIC_RELEASE);

  // Spin until the tracer allows the tracee to proceed
  while (__atomic_load_n(&c->state, __ATOMIC_ACQUIRE) != CHANNEL_STATE_EXIT_PROCEED) {
  }

  // Was a traced syscall IP set?
  if (c->traced_syscall_ip != 0) {
    uint8_t* p = (void*)c->traced_syscall_ip;

    /*char buffer[256];
    int len = snprintf(buffer, 256, "Traced syscall at %p (0x%x 0x%x)\n",
                       (void*)c->traced_syscall_ip, p[-2], p[-1]);
    safe_syscall(__NR_write, 2, buffer, len);*/
  }
}

void channel_release(tracing_channel_t* c) {
  // Reset the channel to available
  __atomic_store_n(&c->state, CHANNEL_STATE_AVAILABLE, __ATOMIC_RELEASE);
}

int open(const char* pathname, int flags, mode_t mode) {
  // Has the injected library been initialized?
  if (initialized) {
    // Yes. Wrap the library call

    // Find an available channel
    tracing_channel_t* c = channel_acquire();

    // Inform the tracer that this command is entering a library call
    channel_enter(c, __NR_open, (uint64_t)pathname, (uint64_t)flags, (uint64_t)mode, 0, 0, 0,
                  __NR_openat);

    // safe_syscall(__NR_write, 2, "open\n", 5);
    int rc = real_open(pathname, flags, mode);
    // safe_syscall(__NR_write, 1, "done\n", 5);

    // Inform the tracer that this command is exiting a library call
    if (rc < 0) {
      channel_exit(c, -errno);
    } else {
      channel_exit(c, rc);
    }

    // Release the channel for use by another library call
    channel_release(c);

    return rc;

  } else {
    // No. Just move along to the library
    return real_open(pathname, flags, mode);
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
 * system call, so we probably need more than one. A page full of these channels should work just
 * fine. The library just has to scan through the array to find a free entry. It could start at a
 * random index (or its TID modulo the array size) and scan forward to find the first free entry.
 * Then just reuse that entry as long as it remains free.
 *
 * Messages:
 * 1. tracee claims a channel by locking it
 * 2. tracee indicates to rkr that it is ready to enter a syscall-issuing libc function
 * 3. rkr allows tracee to proceed to libc function
 * 4. tracee indicates to rkr that it has finished libc function
 * 5. rkr allows trace to resume after libc function---may include information about a syscall ip
 * 6. tracee releases a channel by unlocking it
 */
