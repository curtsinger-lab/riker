.section .data
.global safe_syscall_start
.global safe_syscall_end

# The assembly below will be copied to the safe syscall page to issue untraced syscalls.
# Because it is copied, the assembly must be fully position independent.
safe_syscall_start:
  mov w8, w0
  mov x0, x1
  mov x1, x2
  mov x2, x3
  mov x3, x4
  mov x4, x5
  mov x5, x6
  mov x6, x7
  svc #0x0
  ret
safe_syscall_end:
