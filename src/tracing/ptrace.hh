#pragma once

#include <memory>
#include <vector>

#include <sys/types.h>

class Command;
class Tracer;

struct InitialFdEntry {
  int parent_fd;
  int child_fd;
};

pid_t start_command(std::shared_ptr<Command> cmd, std::vector<InitialFdEntry> initial_fds);
void trace_step(Tracer& tracer, pid_t child, int wait_status);
