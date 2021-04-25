#include <stdio.h>

extern void a() __attribute__((weak));
extern void b() __attribute__((weak));
extern void c() __attribute__((weak));

int main() {
  printf("Hello from main.\n");
  if (a) a();
  if (b) b();
  if (c) c();

  return 0;
}
