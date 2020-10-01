#include <errno.h>
#include <stdio.h>
#include <unistd.h>

/*
 * This test is inspired by a failing case from rename.m4
 * Prior to the run, file1 and file2 are set up to be links to the same inode.
 * Renaming file1 to file2 should succeed, but not remove either entry.
 */

int main() {
  // If file1 and file2 are links to the same inode, both remain
  int rc = rename("file1", "file2");
  if (rc) {
    printf("rename(\"file1\", \"file2\") failed\n");
  }

  // Remove file2. If the previous condition holds, file1 still exists
  rc = unlink("file2");
  if (rc) {
    printf("unlink(\"file2\") failed\n");
  }

  // Rename file1 to itself to check for existence.
  rc = rename("file1", "file1");
  if (rc) {
    printf("rename(\"file1\", \"file1\") failed\n");
  }

  // Rename file2 to itself to check for existence.
  rc = rename("file2", "file2");
  if (rc) {
    printf("rename(\"file2\", \"file2\") failed\n");
  }

  return 0;
}
