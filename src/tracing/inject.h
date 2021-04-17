#pragma once

#include <stdint.h>

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

typedef struct tracing_channel {
  uint8_t state;
  long syscall_number;
} tracing_channel_t;
