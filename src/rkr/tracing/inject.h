#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <sys/user.h>

// The page that must contain code to issue untraced system calls
#define SAFE_SYSCALL_PAGE ((void*)0x77770000)

// The known file descriptor used to map the tracing channel shared memory
#define TRACING_CHANNEL_FD 77

// The number of tracing channel entries
#define TRACING_CHANNEL_COUNT 10

// The size of a data buffer available in each tracing channel
#define TRACING_CHANNEL_BUFFER_SIZE 512

// A special pointer value that indicates the tracing channel buffer should be used
#define TRACING_CHANNEL_BUFFER_PTR -77

// Define channel states used to coordinate between tracer and tracee
#define CHANNEL_STATE_AVAILABLE 0  // The channel is available for use by any tracee
#define CHANNEL_STATE_ACQUIRED 1   // The channel is claimed
#define CHANNEL_STATE_WAITING 2    // The tracee is waiting on this channel
#define CHANNEL_STATE_PROCEED 3    // The tracee may proceed

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
  bool syscall_entry;
  bool stop_on_exit;
  bool exit_instead;
  int tid;
  struct user_regs_struct regs;
  long return_value;
  char buffer[TRACING_CHANNEL_BUFFER_SIZE];
} tracing_channel_t;

struct shared_tracing_data {
  uint64_t in_use;
  uint64_t _padding1[7];
  uint64_t waiting;
  uint64_t _padding2[7];
  tracing_channel_t channels[TRACING_CHANNEL_COUNT];
};
