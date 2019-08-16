#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

int main() {
  // read a file
  FILE *f1 = fopen("inputA", "r");
  char buf[6];
  fgets(buf, 6, f1);
  
  // write to a new file
  FILE *f2 = fopen("myfile", "w");
  fputs(buf, f2);
  fclose(f2);

  // spawn a second writer
  pid_t pid = fork();
    
  if (pid == -1) {
    // badness
    return EXIT_FAILURE;
  } else if (pid > 0) {
    // parent
    int status;
    return EXIT_SUCCESS;
  } else {
    // child
    char* argv[] = { NULL };
    char* envp[] = { NULL };
    execve("./B", argv, envp);
    // should never be called
    _exit(EXIT_FAILURE);
  }
}
