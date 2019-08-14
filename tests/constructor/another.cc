#include <stdio.h>
#include <vector>

struct Foo {
  Foo() : x(1) {
    printf("I am here.\n");
  }
  int x;
};

__attribute__((constructor)) void init2() {
  printf("Here's another constructor.\n");
}

Foo f, g, h;
std::vector<Foo> v;

