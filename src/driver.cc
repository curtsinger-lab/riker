#include <memory>
#include <iostream>
#include <kj/vector.h>

#include <cerrno>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>

#include "middle.h"
#include "trace.h"
#include "dodorun.h"

/**
 * This is the entry point for the 'dodo' trace program.
 */

int main(int argc, char* argv[]) {
    bool use_fingerprints = true;
    std::set<std::string> explicitly_changed;
    std::set<std::string> explicitly_unchanged;
    bool dry_run = false;
    size_t parallel_jobs = 1;
    bool visualize = false;
    bool show_sysfiles = false;
    bool show_collapsed = true;
    for (int i = 1; i < argc; i++) {
        auto arg = std::string(argv[i]);
        if (arg == "--no-fingerprints") {
            use_fingerprints = false;
        } else if (arg == "--changed") {
            if (i + 1 < argc) {
                i++;
                explicitly_changed.insert(std::string(argv[i]));
            } else {
                std::cerr << "Please specify a file to mark as changed" << std::endl;
                exit(1);
            }
        } else if (arg == "--unchanged") {
            if (i + 1 < argc) {
                i++;
                explicitly_unchanged.insert(std::string(argv[i]));
            } else {
                std::cerr << "Please specify a file to mark as unchanged" << std::endl;
                exit(1);
            }
        } else if (arg == "--dry-run") {
            dry_run = true;
        } else if (arg == "-j") {
            if (i + 1 < argc) {
                i++;
                long specified_jobs = std::stol(std::string(argv[i]));
                if (specified_jobs < 1)  {
                    std::cerr << "Invalid number of jobs: specify at least one" << std::endl;
                    exit(1);
                }
                parallel_jobs = specified_jobs;
            } else {
                std::cerr << "Please specify a number of jobs to use" << std::endl;
                exit(1);
            }
        } else if (arg == "--visualize") {
            visualize = true;
        } else if (arg.size() >= 18 && arg.substr(0, 18) == "--visualize-files=") {
            visualize = true;
            auto file_ty = arg.substr(18);
            if (file_ty == "local") {
                show_sysfiles = false;
            } else if (file_ty == "all") {
                show_sysfiles = true;
            } else {
                std::cerr << "Invalid argument to --visualize-files: options are local [default] and all" << std::endl;
                exit(1);
            }
        } else if (arg.size() >= 21 && arg.substr(0, 21) == "--visualize-clusters=") {
            visualize = true;
            auto cluster_ty = arg.substr(21);
            if (cluster_ty == "expand") {
                show_collapsed = true;
            } else if (cluster_ty == "collapse") {
                show_collapsed = false;
            } else {
                std::cerr << "Invalid argument to --visualize-clusters: options are expand [default] and collapse" << std::endl;
                exit(1);
            }
        } else {
            std::cerr << "Invalid argument " << arg << std::endl;
            exit(1);
        }
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

    int db_fd = open("db.dodo", O_RDWR);
    if (db_fd >= 0) {
        RebuildState rebuild_state(db_fd, use_fingerprints, explicitly_changed, explicitly_unchanged);
        rebuild_state.rebuild(use_fingerprints, dry_run, parallel_jobs);
        if (visualize) {
            rebuild_state.visualize(show_sysfiles, show_collapsed);
        }
        return 0;
    }

    auto root_cmd = new Command(&*state, kj::heapArray((const kj::byte*) "Dodofile", 8), nullptr, 0);
    root_cmd->args.push_back(kj::heapArray((const kj::byte*) "Dodofile", 8));
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
