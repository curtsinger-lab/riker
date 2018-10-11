#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/user.h>

int main() {
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

    for(int i=0; i<10; i++) {
      printf("child: %d\n", i);
      sleep(1);
    }
    exit(0);

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
        printf("\t\t\tChild received signal %d. Delivering.\n", WSTOPSIG(status));
        last_signal = WSTOPSIG(status);

        if(last_signal == SIGTRAP) {
          printf("\t\t\tCaught a syscall.\n");

          // Read register state from the child process
          struct user_regs_struct regs;
          if(ptrace(PTRACE_GETREGS, child_pid, NULL, &regs)) {
            perror("ptrace GETREGS failed");
            exit(2);
          }

          printf("\t\t\trax is %llu\n", regs.orig_rax);

          last_signal = 0;
          getchar();
        }
      }
    }
  }
}

