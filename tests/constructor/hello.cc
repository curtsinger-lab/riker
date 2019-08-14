#include <stdio.h>

__attribute__((constructor)) void init() {
  printf("Starting up.\n");
}

int main() {
  printf("Hello world!!\n");
  return 0;
}

