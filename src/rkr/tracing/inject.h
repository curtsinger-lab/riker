#pragma once

#include <semaphore.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/user.h>

// The page that must contain code to issue untraced system calls
#define SAFE_SYSCALL_PAGE ((void*)0x77770000)

// The known file descriptor used to map the tracing channel shared memory
#define TRACING_CHANNEL_FD 77

// The number of tracing channel entries
#define TRACING_CHANNEL_COUNT 32

// The size of a data buffer available in each tracing channel
#define TRACING_CHANNEL_BUFFER_SIZE 4096

// A special pointer value that indicates the tracing channel buffer should be used
#define TRACING_CHANNEL_BUFFER_PTR 0x7777777700000000

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

/**
 * Channels always have a single state. The options are:
 * - available: the channel is not in use and can be acquired by any tracee
 * - acquired: a tracee owns the channel but it is not waiting on the tracer yet
 * - pre-syscall wait: a tracee is waiting before issuing a system call
 * - post-syscall notify: a tracee has finished a system call but is not blocking
 * - post-syscall wait: a tracee has finished a system call and is waiting to be resumed
 * - proceed: the tracee can unblock
 * - observed: the tracer has observed the state of the channel and is currently handling it
 */

#define CHANNEL_STATE_AVAILABLE 0
#define CHANNEL_STATE_ACQUIRED 1
#define CHANNEL_STATE_PRE_SYSCALL_WAIT 2
#define CHANNEL_STATE_POST_SYSCALL_NOTIFY 3
#define CHANNEL_STATE_POST_SYSCALL_WAIT 4
#define CHANNEL_STATE_PROCEED 5
#define CHANNEL_STATE_OBSERVED 6

/********** Channel Actions **********/

/**
 * When the tracer resumes a tracee it can request that the trace perform one of the following
 * actions:
 * - continue: the tracee can run the syscall and does not have to block
 * - notify: the tracee can run the syscall and should report the result without blocking
 * - finish: the tracee should run the syscall and block again
 * - exit: the tracee should exit instead of running the system call
 * - skip: the tracee should skip the system call and use the return value from the channel
 */

#define CHANNEL_ACTION_CONTINUE 0
#define CHANNEL_ACTION_NOTIFY 1
#define CHANNEL_ACTION_FINISH 2
#define CHANNEL_ACTION_EXIT 3
#define CHANNEL_ACTION_SKIP 4

typedef struct tracing_channel {
  sem_t wake_tracee;
  uint8_t state;
  uint8_t action;
  int tid;
  struct user_regs_struct regs;
  size_t buffer_pos;
  char buffer[TRACING_CHANNEL_BUFFER_SIZE];
} tracing_channel_t;

struct shared_tracing_data {
  sem_t available;
  tracing_channel_t channels[TRACING_CHANNEL_COUNT];
};
