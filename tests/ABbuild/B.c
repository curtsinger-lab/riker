#include <stdio.h>

int main() {
  // read a file
  FILE *f1 = fopen("inputB", "r");
  char buf[8];
  fgets(buf, 8, f1);
  
  // write to a new file
  FILE * f2 = fopen("myfile", "a");
  fputs(buf, f2);
  fclose(f2);

  return 0;
}
