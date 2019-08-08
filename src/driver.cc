#include <memory>
#include <iostream>
#include <kj/vector.h>

#include <cerrno>
#include <sys/wait.h>
#include <unistd.h>

#include "middle.h"
#include "trace.h"

/**
 * This is the entry point for the 'dodo' trace program.
 */

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << std::string(argv[0]) << " <shell command>" << std::endl;
        return 1;
    }

    auto state = std::make_unique<trace_state>();
    kj::Vector<kj::byte> cwd(16);
    while (true) { // Loop because we have no working upper bound on path length
        cwd.resize(cwd.capacity());
        if (getcwd(cwd.asPtr().asChars().begin(), cwd.size()) != nullptr) {
            cwd.resize(strlen((char*) cwd.begin()));
            break;
        }
        cwd.reserve(cwd.capacity() + 1);
    }
    state->starting_dir = cwd.releaseAsArray();

    auto root_cmd = new Command(&*state, kj::heapArray((const kj::byte*) "/bin/sh", 7), nullptr, 0);
    root_cmd->args.push_back(kj::heapArray((const kj::byte*) "sh", 2));
    root_cmd->args.push_back(kj::heapArray((const kj::byte*) "-c", 2));
    root_cmd->args.push_back(kj::heapArray((const kj::byte*) argv[1], strlen(argv[1])));
    state->commands.push_front(root_cmd);

    // TODO: set up stdio for logging?
    start_command(root_cmd, kj::ArrayPtr<InitialFdEntry const>());

    while (true) {
        int wait_status;
        pid_t child = wait(&wait_status);
        if (child == -1) {
            if (errno == ECHILD) {
                // ECHILD is returned when there are no children to wait on, which
                // is by far the simplest and most reliable signal we have for when
                // to exit (cleanly).
                break;
            } else {
                perror("Error while waiting");
                exit(2);
            }
        }

        trace_step(&*state, child, wait_status);
    }

    state->serialize_graph();
}
