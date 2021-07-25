.section .data
.global safe_syscall_start
.global safe_syscall_end

# The assembly below will be copied to the safe syscall page to issue untraced syscalls.
# Because it is copied, the assembly must be fully position independent.
safe_syscall_start:
  endbr64
  mov %rdi, %rax
  mov %rsi, %rdi
  mov %rdx, %rsi
  mov %rcx, %rdx
  mov %r8, %r10
  mov %r9, %r8
  mov 0x8(%rsp), %r9
  syscall
  retq
safe_syscall_end:
