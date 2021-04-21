#pragma once

#include <stdint.h>
#include <sys/user.h>

// The page that must contain code to issue untraced system calls
#define SAFE_SYSCALL_PAGE ((void*)0x77770000)

// The known file descriptor used to map the tracing channel shared memory
#define TRACING_CHANNEL_FD 77

// The size of the tracing channel shared mappy
#define TRACING_CHANNEL_SIZE 0x1000

// Define channel states used to coordinate between tracer and tracee
#define CHANNEL_STATE_AVAILABLE 0      // The channel is available for use by any process
#define CHANNEL_STATE_ACQUIRED 1       // The channel is claimed by a process
#define CHANNEL_STATE_ENTRY 2          // The tracee is waiting before entry to the library call
#define CHANNEL_STATE_ENTRY_PROCEED 3  // The tracee can proceed with the library call
#define CHANNEL_STATE_EXIT 4           // The tracee is waiting after exit from the library call
#define CHANNEL_STATE_EXIT_PROCEED 5   // The tracee can resume running after the library call

// Register meanings on syscall entry
#define INSTRUCTION_POINTER rip
#define SYSCALL_NUMBER orig_rax
#define SYSCALL_RETURN rax
#define SYSCALL_ARG1 rdi
#define SYSCALL_ARG2 rsi
#define SYSCALL_ARG3 rdx
#define SYSCALL_ARG4 r10
#define SYSCALL_ARG5 r8
#define SYSCALL_ARG6 r9

typedef struct tracing_channel {
  uint8_t state;
  int tid;
  struct user_regs_struct regs;
  long alternate_syscall;
  long return_value;
  uintptr_t traced_syscall_ip;
} tracing_channel_t;
