#define _GNU_SOURCE
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>

#define FILENAME "file"
#define MODE 0777

int flags[] = { O_APPEND, O_ASYNC, O_CLOEXEC, O_CREAT, O_DIRECT, O_DIRECTORY, O_DSYNC, O_EXCL, O_LARGEFILE, O_NOATIME, O_NOCTTY, O_NOFOLLOW, O_NONBLOCK, O_PATH, O_SYNC, O_TMPFILE, O_TRUNC};
char *flagnames[] = { "O_APPEND", "O_ASYNC", "O_CLOEXEC", "O_CREAT", "O_DIRECT", "O_DIRECTORY", "O_DSYNC", "O_EXCL", "O_LARGEFILE", "O_NOATIME", "O_NOCTTY", "O_NOFOLLOW", "O_NONBLOCK", "O_PATH", "O_SYNC", "O_TMPFILE", "O_TRUNC"};
int numflags = sizeof(flags) / sizeof(O_APPEND);

int getFlagMask(int myflags) {
  int flags_set = 0;
  
  for (int i = 0; i < numflags; i++) {
    bool is_set = (myflags & (1 << i)) == (1 << i);
    if (is_set) flags_set |= flags[i];
  }
  
  return flags_set;
}

void getFlagString(int myflags, char **buf) {
  char *s = *buf;
  bool first = true;
  
  for (int i = 0; i < numflags; i++) {
    bool is_set = (myflags & (1 << i)) == (1 << i);
    if (is_set) {
      int cursz = s == NULL ? 0 : strlen(s);
      int nbytes = cursz + strlen(flagnames[i]) + (first ? 1 : 2);
      char *s2 = malloc(nbytes);
      memset(s2, 0, nbytes);
      if (s) strcat(s2, s);
      if (!first) strcat(s2, "|");
      strcat(s2, flagnames[i]);
      free(s);
      s = s2;
      first = false;
    }
  }
  if (s == NULL) {
    s = malloc(1);
    memset(s, 0, 1);
  }
  *buf = s;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("Usage: %s <nth combination of flags>\n", argv[0]);
    exit(1);
  }
  int numcombos = 2 << (numflags-1);
  int num = atoi(argv[1]) % numcombos;

  char *buf = NULL;
  int m = getFlagMask(num);
  getFlagString(num, &buf);
  printf("%s (%d)\n", buf, m);

  // run experiment
  int fd = open(FILENAME, m, MODE);
  if (fd == -1) {
    printf("Invalid syscall: open(\"%s\", %s, %o)\n\t%s\n", FILENAME, buf, MODE, strerror(errno));
  }
  
  free(buf);

  return 0;
}
