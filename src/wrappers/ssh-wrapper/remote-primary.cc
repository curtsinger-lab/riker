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
  // get the pid of remote ssh sessions
  int remote_pid_i = getpid();
  char remote_pid_ch[10];

  // convert in to string
  snprintf(remote_pid_ch, sizeof(remote_pid_ch), "%d", remote_pid_i);

  int rc = fork();
  if (rc == 0) {
    // send both the length and the pid to local channel
    printf("%ld", strlen(remote_pid_ch));
    printf("%d", remote_pid_i);
  }
  if (rc > 0) {
    // wait until the child process is finished
    wait(NULL);

    // create string to write to
    const char* before = "/proc/";
    const char* after = "/fd/0";
    char command[strlen(before) + strlen(after) + strlen(remote_pid_ch) + 1];

    // open fd to receive command from secondary channel
    strcpy(command, before);
    strcat(command, remote_pid_ch);
    strcat(command, after);
    int fdi = open(command, O_RDONLY);

    // read end signal from secondary channel to check if it finished
    char end_sig[2];
    read(fdi, end_sig, 2);

    close(fdi);
  }
}