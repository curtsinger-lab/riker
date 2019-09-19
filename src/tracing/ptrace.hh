#pragma once

#include "core/BuildGraph.hh"

struct InitialFdEntry {
  int parent_fd;
  int child_fd;
};

pid_t start_command(BuildGraph& trace, Command* cmd, kj::ArrayPtr<InitialFdEntry const> initial_fds);
void trace_step(BuildGraph& trace, pid_t child, int wait_status);
