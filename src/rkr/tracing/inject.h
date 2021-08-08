#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <sys/user.h>

// The page that must contain code to issue untraced system calls
#define SAFE_SYSCALL_PAGE ((void*)0x77770000)

// The known file descriptor used to map the tracing channel shared memory
#define TRACING_CHANNEL_FD 77

// The number of tracing channel entries
#define TRACING_CHANNEL_COUNT 8

// The size of a data buffer available in each tracing channel
#define TRACING_CHANNEL_BUFFER_SIZE 4096

// A special pointer value that indicates the tracing channel buffer should be used
#define TRACING_CHANNEL_BUFFER_PTR -77

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

/********** Channel States **********/

// The channel is available for use
#define CHANNEL_STATE_AVAILABLE 0

// An acquired channel is owned by a tracee
#define CHANNEL_STATE_ACQUIRED 1

// A tracee is waiting before entering a system call
#define CHANNEL_STATE_ENTRY_WAITING 2

// The tracer has observed a tracee waiting before a system call
#define CHANNEL_STATE_ENTRY_SEEN 3

// The tracee may proceed with the system call
#define CHANNEL_STATE_ENTRY_PROCEED 4

// A tracee is waiting after exiting a system call
#define CHANNEL_STATE_EXIT_WAITING 5

// The tracer has observed a tracee waiting after a system call
#define CHANNEL_STATE_EXIT_SEEN 6

// The tracee may resume executing after a system call
#define CHANNEL_STATE_EXIT_PROCEED 7

typedef struct tracing_channel {
  uint8_t state;
  int tid;
  struct user_regs_struct regs;
  bool stop_on_exit;
  bool exit_instead;
  long return_value;
  char buffer[TRACING_CHANNEL_BUFFER_SIZE];
} tracing_channel_t;

struct shared_tracing_data {
  tracing_channel_t channels[TRACING_CHANNEL_COUNT];
};
