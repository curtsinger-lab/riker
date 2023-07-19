#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include <bits/stdc++.h>

using namespace std;

int main(int argc, char* argv[]) {
  int remote_pid_i = getpid();
  char remote_pid_ch[10];
  snprintf(remote_pid_ch, sizeof(remote_pid_ch), "%d", remote_pid_i);

  int rc = fork();
  if (rc == 0) {
    // printf("%ld", strlen(remote_pid_ch));
    printf("7");
    printf("%d", remote_pid_i);
  }
  if (rc > 0) {
    wait(NULL);
    char end_sig[2];

    const char* before = "/proc/";
    char command[100];
    const char* after = "/fd/0";

    strcpy(command, before);
    strcat(command, remote_pid_ch);
    strcat(command, after);
    int fdi = open(command, O_RDONLY);
    read(fdi, end_sig, 2);

    printf("All done\n");

    close(fdi);
  }
}