#pragma once

// Register meanings on syscall entry
#define INSTRUCTION_POINTER rip
#define SYSCALL_NUMBER regs[8]
#define SYSCALL_RETURN regs[0]
#define SYSCALL_ARG1 regs[0]
#define SYSCALL_ARG2 regs[1]
#define SYSCALL_ARG3 regs[2]
#define SYSCALL_ARG4 regs[3]
#define SYSCALL_ARG5 regs[4]
#define SYSCALL_ARG6 regs[5]
