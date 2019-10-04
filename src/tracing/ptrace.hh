#pragma once

#include <memory>
#include <vector>

#include <sys/types.h>

class Command;
class Tracer;

using std::shared_ptr;
using std::vector;

struct InitialFdEntry {
  int parent_fd;
  int child_fd;
};

pid_t start_command(Command* cmd, vector<InitialFdEntry> initial_fds);
void trace_step(Tracer& tracer, pid_t child, int wait_status);
