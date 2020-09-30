#include <signal.h>

int main() {
  raise(SIGABRT);
}