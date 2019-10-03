#pragma once

#include <memory>

#include <sys/types.h>

#include <kj/common.h>

class Command;
class Tracer;

struct InitialFdEntry {
  int parent_fd;
  int child_fd;
};

pid_t start_command(std::shared_ptr<Command> cmd, kj::ArrayPtr<InitialFdEntry const> initial_fds);
void trace_step(Tracer& tracer, pid_t child, int wait_status);
