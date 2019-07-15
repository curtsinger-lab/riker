#include <memory>
#include <iostream>
#include <kj/vector.h>
#include <unistd.h>

#include "middle.h"
#include "trace.h"

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

    auto root_cmd = new Command(&*state, kj::heapArray((kj::byte*) argv[1], strlen(argv[1])), nullptr, 0);
    state->commands.push_front(root_cmd);

    run_command(root_cmd, argv[1]);

    state->serialize_graph();
}
