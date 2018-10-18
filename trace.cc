#include <cstdio>
#include <cstdlib>
#include <set>

#include <unistd.h>
#include <asm/unistd.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/user.h>

using std::set;

// Comparator procedure for sorting an array of size_ts
int compare_size_t(const void* a, const void* b) {
  return *(size_t*)a - *(size_t*)b;
}

int main(int argc, char** argv) {
  if(argc == 1) {
    printf("Usage: %s command [args...]\n", argv[0]);
    exit(1);
  }

  pid_t child_pid = fork();
  if(child_pid == -1) {
    perror("fork failed");
    exit(2);
  }

  if(child_pid == 0) {
    if(ptrace(PTRACE_TRACEME, 0, NULL, NULL) == -1) {
      perror("ptrace traceme failed");
      exit(2);
    }

    // Stop the process so the tracer can catch it
    raise(SIGSTOP);

    // Set up to run the subcommand specified in argv.
    // Copy the given arguments to a new argv array, leaving space for the NULL terminator
    int new_argc = argc - 1;
    char** new_argv = new char*[new_argc + 1];
    for(int i=0; i<new_argc; i++) {
      new_argv[i] = argv[i+1];
    }

    // NULL-terminate the new argv array
    new_argv[new_argc] = NULL;

    if(execvp(new_argv[0], new_argv)) {
      perror("exec failed");
      exit(2);
    }

  } else {
    // Wait for the child to stop
    int status;
    int result;
    do {
      result = waitpid(child_pid, &status, 0);
      if(result != child_pid) {
        perror("waitpid failed");
        exit(2);
      }
    } while(!WIFSTOPPED(status));

    printf("\t\t\tAttached!\n");

    bool running = true;
    int last_signal = 0;
    set<size_t> skipped_syscalls;

    while(running) {
      // Continue the process, delivering the last signal we received (if any)
      if(ptrace(PTRACE_SYSCALL, child_pid, NULL, last_signal) == -1) {
        perror("ptrace CONT failed");
        exit(2);
      }

      // No signal to send yet
      last_signal = 0;

      // Wait for the child to stop
      if(waitpid(child_pid, &status, 0) != child_pid) {
        perror("waitpid failed");
        exit(2);
      }

      if(WIFEXITED(status)) {
        printf("\t\t\tChild exited with status %d\n", WEXITSTATUS(status));
        running = false;
      } else if(WIFSIGNALED(status)) {
        printf("\t\t\tChild terminated with signal %d\n", WTERMSIG(status));
        running = false;
      } else if(WIFSTOPPED(status)) {
        // Get the signal delivered to the child
        last_signal = WSTOPSIG(status);

        // If the signal was a SIGTRAP, we stopped because of a system call
        if(last_signal == SIGTRAP) {
          // Read register state from the child process
          struct user_regs_struct regs;
          if(ptrace(PTRACE_GETREGS, child_pid, NULL, &regs)) {
            perror("ptrace GETREGS failed");
            exit(2);
          }

          // Get the system call number
          size_t syscall_num = regs.orig_rax;

          if(syscall_num == __NR_open) {
            printf("\t\t\topen(...)\n");

          } else if(syscall_num == __NR_close) {
            printf("\t\t\tclose(...)\n");

          } else if(syscall_num == __NR_stat) {
            printf("\t\t\tstat(...)\n");
          
          } else if(syscall_num == __NR_fstat) {
            printf("\t\t\tfstat(...)\n");

          } else if(syscall_num == __NR_lstat) {
            printf("\t\t\tlstat(...)\n");

          } else if(syscall_num == __NR_access) {
            printf("\t\t\taccess(...)\n");

          } else if(syscall_num == __NR_openat) {
            printf("\t\t\topenat(...)\n");

          } else {
            // Add the syscall number to the set of skipped syscalls
            skipped_syscalls.insert(syscall_num);
          }

          // Also trace:
          //  exec
          //  clone (fork/pthread_create)
          //  mkdir
          //  rmdir
          //  creat
          //  rename?
          //  link
          //  unlink
          //  symlink?
          //  readlink?
          //  chmod variants?
          //  xattrs?
          //  Weird system-level changes like chroot, mount, umount, etc. could be important later
          //  openat, mkdirat, ___at syscalls

          last_signal = 0;
        }
      }
    }

    // Print the list of skipped syscalls
    printf("There were %lu unique unhandled syscalls.\n", skipped_syscalls.size());
    for(size_t syscall_num : skipped_syscalls) {
      printf("  %lu\n", syscall_num);
    }
  }
}

