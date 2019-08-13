#pragma once

#include "middle.h"

struct InitialFdEntry {
    int parent_fd;
    int child_fd;
};

pid_t start_command(Command* cmd, kj::ArrayPtr<InitialFdEntry const> initial_fds);
void trace_step(trace_state* state, pid_t child, int wait_status);
