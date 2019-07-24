#pragma once

#include "middle.h"

struct InitialFdEntry {
    int parent_fd;
    int child_fd;
};

void run_command(Command* cmd, kj::ArrayPtr<InitialFdEntry const> initial_fds);
