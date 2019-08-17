
#include <memory>
#include <iostream>
#include <kj/vector.h>

#include <cerrno>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <spawn.h>

#include "middle.h"
#include "trace.h"
#include "dodorun.h"

// Escape a string for correct printing as an argument or command on the shell
static void write_shell_escaped(std::ostream& out_stream, const std::string& input) {
    if (input.find_first_of(" \t\n&();|<>!{}'\"") == std::string::npos &&
        input != std::string("elif") &&
        input != std::string("fi") &&
        input != std::string("while") &&
        input != std::string("case") &&
        input != std::string("else") &&
        input != std::string("for") &&
        input != std::string("then") &&
        input != std::string("do") &&
        input != std::string("done") &&
        input != std::string("until") &&
        input != std::string("if") &&
        input != std::string("esac")) {
        out_stream << input;
        return;
    }

    out_stream << '\'';
    size_t escaped_so_far = 0;
    while (true) {
        size_t quote_offset = input.find('\'', escaped_so_far);
        if (quote_offset == std::string::npos) {
            out_stream.write(input.data() + escaped_so_far, input.size() - escaped_so_far);
            out_stream << '\'';
            return;
        } else {
            out_stream.write(input.data() + escaped_so_far, quote_offset - escaped_so_far);
            out_stream << "'\\''";
            escaped_so_far = quote_offset + 1;
        }
    }
}

/**
 * This is the entry point for the 'dodo' trace program.
 */

int main(int argc, char* argv[]) {
    //bool use_fingerprints = true;
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

    // Although the documentation recommends against this, we implicitly trust the database
    // anyway.
    ::capnp::ReaderOptions options;
    options.traversalLimitInWords = std::numeric_limits<uint64_t>::max();

    int db_fd = open("db.dodo", O_RDWR);
    if (db_fd >= 0) {
        ::capnp::StreamFdMessageReader message(db_fd, options);
        auto old_graph = message.getRoot<db::Graph>();
        auto old_files = old_graph.getFiles();
        auto old_commands = old_graph.getCommands();
        RebuildState rebuild_state(old_graph, use_fingerprints, explicitly_changed, explicitly_unchanged);

        pid_t dry_run_pid = 1;
        std::map<pid_t, old_command*> wait_worklist;
        while (true) {
            auto run_command = rebuild_state.rebuild(use_fingerprints, dry_run, wait_worklist.size(), parallel_jobs);
            if (run_command == nullptr) {
                if (wait_worklist.empty()) {
                    // We're done!
                    break;
                } else {
                    pid_t child;
                    if (dry_run) {
                        child = wait_worklist.begin()->first;
                    } else {
                        int wait_status;
                        child = wait(&wait_status);
                        if (child == -1) {
                            perror("Error while waiting");
                            exit(2);
                        }

                        trace_step(&*state, child, wait_status);
                        if (!WIFEXITED(wait_status) && !WIFSIGNALED(wait_status)) {
                            continue;
                        }
                    }

                    auto child_entry = wait_worklist.find(child);
                    if (child_entry == wait_worklist.end()) {
                        std::cerr << "Unrecognized process ended: " << child << std::endl;
                    } else {
                        old_command* child_command = child_entry->second;
                        wait_worklist.erase(child_entry);

                        rebuild_state.mark_complete(use_fingerprints, dry_run, child_command);
                    }
                    continue;
                }
            }

            // Print that we will run it
            write_shell_escaped(std::cout, run_command->executable);
            for (size_t arg_index = 1; arg_index < run_command->args.size(); arg_index++) {
                std::cout << " ";
                write_shell_escaped(std::cout, run_command->args[arg_index]);
            }
            // Print redirections
            for (auto initial_fd_entry : old_commands[run_command->id].getInitialFDs()) {
                std::cout << " ";
                if   (!(initial_fd_entry.getFd() == fileno(stdin) && initial_fd_entry.getCanRead() && !initial_fd_entry.getCanWrite())
                   && !(initial_fd_entry.getFd() == fileno(stdout) && !initial_fd_entry.getCanRead() && initial_fd_entry.getCanWrite())) {
                    std::cout << initial_fd_entry.getFd();
                }
                if (initial_fd_entry.getCanRead()) {
                    std::cout << '<';
                }
                if (initial_fd_entry.getCanWrite()) {
                    std::cout << '>';
                }
                if (rebuild_state.files[initial_fd_entry.getFileID()]->is_pipe) {
                    std::cout << "/proc/dodo/pipes/" << initial_fd_entry.getFileID();
                } else {
                    write_shell_escaped(std::cout, rebuild_state.files[initial_fd_entry.getFileID()]->path);
                }
            }
            std::cout << std::endl;

            // Run it!
            pid_t child_pid;
            if (dry_run) {
                child_pid = dry_run_pid;
                dry_run_pid++;
            } else {
                // Set up initial fds
                kj::Vector<InitialFdEntry> file_actions;
                std::vector<int> opened_fds;
                int max_fd = 0;
                for (auto initial_fd_entry : old_commands[run_command->id].getInitialFDs()) {
                    int fd = initial_fd_entry.getFd();
                    if (fd > max_fd) {
                        max_fd = fd;
                    }
                }
                for (auto initial_fd_entry : old_commands[run_command->id].getInitialFDs()) {
                    auto file = rebuild_state.files[initial_fd_entry.getFileID()];
                    int open_fd_storage;
                    int* open_fd_ref;
                    if (file->is_pipe) {
                        if (file->scheduled_for_creation) {
                            file->scheduled_for_creation = false;
                            int pipe_fds[2];
                            // FIXME(portability): pipe2 is Linux-specific; fall back to pipe+fcntl?
                            if (pipe2(pipe_fds, O_CLOEXEC) != 0) {
                                perror("Failed to create pipe");
                                continue;
                            }
                            file->pipe_reader_fd = pipe_fds[0];
                            file->pipe_writer_fd = pipe_fds[1];
                        }

                        if (initial_fd_entry.getCanRead()) {
                            open_fd_ref = &file->pipe_reader_fd;
                        } else { // TODO: check for invalid read/write combinations?
                            open_fd_ref = &file->pipe_writer_fd;
                        }
                    } else {
                        int flags = O_CLOEXEC;
                        if (initial_fd_entry.getCanRead() && initial_fd_entry.getCanWrite()) {
                            flags |= O_RDWR;
                        } else if (initial_fd_entry.getCanWrite()) {
                            flags |= O_WRONLY;
                        } else { // TODO: what if the database has no permissions for some reason?
                            flags |= O_RDONLY;
                        }
                        if (file->scheduled_for_creation) {
                            file->scheduled_for_creation = false;
                            flags |= O_CREAT | O_TRUNC;
                        }
                        mode_t mode = old_files[initial_fd_entry.getFileID()].getMode();
                        open_fd_storage = open(file->path.c_str(), flags, mode);
                        if (open_fd_storage == -1) {
                            perror("Failed to open");
                            continue;
                        }
                        open_fd_ref = &open_fd_storage;
                    }
                    // Ensure that the dup2s won't step on each other's toes
                    if (*open_fd_ref <= max_fd) {
                        int new_fd = fcntl(*open_fd_ref, F_DUPFD_CLOEXEC, max_fd + 1);
                        if (new_fd == -1) {
                            perror("Failed to remap FD");
                            continue;
                        }
                        close(*open_fd_ref);
                        *open_fd_ref = new_fd;
                    }
                    if (!file->is_pipe) {
                        opened_fds.push_back(*open_fd_ref);
                    }
                    file_actions.add((InitialFdEntry) {
                        .parent_fd = *open_fd_ref,
                        .child_fd = initial_fd_entry.getFd()
                    });
                }
                // Spawn the child
                auto middle_cmd = new new_command(&*state, kj::heapArray((const kj::byte*) run_command->executable.data(), run_command->executable.size()), nullptr, 0);
                for (size_t arg_index = 0; arg_index < run_command->args.size(); arg_index++) {
                    middle_cmd->args.push_back(kj::heapArray((const kj::byte*) run_command->args[arg_index].data(), run_command->args[arg_index].size()));
                }
                child_pid = start_command(middle_cmd, file_actions);
                // Free what we can
                for (auto open_fd : opened_fds) {
                    close(open_fd);
                }
                for (auto initial_fd_entry : old_commands[run_command->id].getInitialFDs()) {
                    auto file = rebuild_state.files[initial_fd_entry.getFileID()];
                    if (file->is_pipe) {
                        if (initial_fd_entry.getCanRead()) {
                            file->pipe_reader_references -= 1;
                            if (file->pipe_reader_references == 0) {
                                close(file->pipe_reader_fd);
                            }
                        } else {
                            file->pipe_writer_references -= 1;
                            if (file->pipe_writer_references == 0) {
                                close(file->pipe_writer_fd);
                            }
                        }
                    }
                }
            }

            wait_worklist[child_pid] = run_command;
        }
        if (visualize) {
            rebuild_state.visualize(show_sysfiles, show_collapsed);
        }
        return 0;
    }

    auto root_cmd = new new_command(&*state, kj::heapArray((const kj::byte*) "Dodofile", 8), nullptr, 0);
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
