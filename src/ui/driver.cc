#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <forward_list>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <capnp/list.h>
#include <capnp/message.h>
#include <capnp/serialize.h>
#include <kj/common.h>
#include <kj/vector.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/dodorun.hh"
#include "core/Serializer.hh"
#include "db/db.capnp.h"
#include "tracing/ptrace.hh"
#include "ui/log.hh"
#include "ui/options.hh"
#include "ui/util.hh"

using std::forward_list;
using std::string;

// Declare the global command-line options struct
dodo_options options;

/**
 * Parse command line options and return a dodo_options struct.
 */
void parse_argv(forward_list<string> argv) {
  // Loop until we've consumed all command line arguments
  while (!argv.empty()) {
    // Take the first argument off the list
    string arg = argv.front();
    argv.pop_front();

    if (arg == "--debug") {
      options.log_source_locations = true;
      options.log_threshold = log_level::Info;

    } else if (arg == "-v") {
      options.log_threshold = log_level::Warning;

    } else if (arg == "-vv") {
      options.log_threshold = log_level::Info;

    } else if (arg == "-vvv") {
      options.log_threshold = log_level::Verbose;

    } else if (arg == "--no-fingerprints") {
      options.use_fingerprints = false;

    } else if (arg == "--changed") {
      if (!argv.empty()) {
        options.explicitly_changed.insert(argv.front());
        argv.pop_front();
      } else {
        std::cerr << "Please specify a file to mark as changed.\n";
        exit(1);
      }

    } else if (arg == "--unchanged") {
      if (!argv.empty()) {
        options.explicitly_unchanged.insert(argv.front());
        argv.pop_front();
      } else {
        std::cerr << "Please specify a file to mark as unchanged.\n";
        exit(1);
      }

    } else if (arg == "--dry-run") {
      options.dry_run = true;

    } else if (arg == "-j") {
      if (!argv.empty()) {
        long specified_jobs = std::stol(argv.front());
        argv.pop_front();

        if (specified_jobs < 1) {
          std::cerr << "Invalid number of jobs: specify at least one.\n";
          exit(1);
        }
        options.parallel_jobs = specified_jobs;
      } else {
        std::cerr << "Please specify a number of jobs to use" << std::endl;
        exit(1);
      }

    } else if (arg == "--visualize") {
      options.visualize = true;

    } else if (arg == "--visualize-all") {
      options.visualize = true;
      options.show_sysfiles = true;

    } else if (arg == "--hide-collapsed") {
      options.show_collapsed = false;

    } else {
      std::cerr << "Invalid argument " << arg << std::endl;
      exit(1);
    }
  }
}

/**
 * This is the entry point for the dodo command line tool
 */
int main(int argc, char* argv[]) {
  // Parse command line options
  parse_argv(forward_list<string>(argv + 1, argv + argc));

  // Get the current working directory
  char* cwd = getcwd(nullptr, 0);
  FAIL_IF(cwd == nullptr) << "Failed to get current working directory: " << ERR;

  // Create a managed reference to a trace state
  BuildGraph graph(cwd);

  // Clean up after getcwd
  free(cwd);

  // Although the documentation recommends against this, we implicitly trust the
  // database anyway. Without this we may hit the recursion limit.
  ::capnp::ReaderOptions capnp_options;
  capnp_options.traversalLimitInWords = std::numeric_limits<uint64_t>::max();

  // Open the database
  int db_fd = open("db.dodo", O_RDWR);

  // If the database exists, read it
  if (db_fd >= 0) {
    ::capnp::StreamFdMessageReader message(db_fd, capnp_options);
    auto old_graph = message.getRoot<db::Graph>();
    auto old_files = old_graph.getFiles();
    auto old_commands = old_graph.getCommands();
    RebuildState rebuild_state(old_graph, options.use_fingerprints, options.explicitly_changed,
                               options.explicitly_unchanged);

    pid_t dry_run_pid = 1;
    std::map<pid_t, old_command*> wait_worklist;
    while (true) {
      auto run_command = rebuild_state.rebuild(options.use_fingerprints, options.dry_run,
                                               wait_worklist.size(), options.parallel_jobs);
      if (run_command == nullptr) {
        if (wait_worklist.empty()) {
          // We're done!
          break;
        } else {
          pid_t child;
          if (options.dry_run) {
            child = wait_worklist.begin()->first;
          } else {
            int wait_status;
            child = wait(&wait_status);
            FAIL_IF(child == -1) << "Error waiting for child: " << ERR;

            trace_step(graph, child, wait_status);
            if (!WIFEXITED(wait_status) && !WIFSIGNALED(wait_status)) {
              continue;
            }
          }

          auto child_entry = wait_worklist.find(child);
          if (child_entry != wait_worklist.end()) {
            old_command* child_command = child_entry->second;
            wait_worklist.erase(child_entry);

            rebuild_state.mark_complete(options.use_fingerprints, options.dry_run, child_command);
          }
          continue;
        }
      }

      // Print that we will run it
      write_shell_escaped(std::cout, run_command->executable);
      for (auto arg : run_command->args) {
        std::cout << " ";
        write_shell_escaped(std::cout, arg);
      }

      // Print redirections
      for (auto initial_fd_entry : old_commands[run_command->id].getInitialFDs()) {
        std::cout << " ";
        if (!(initial_fd_entry.getFd() == fileno(stdin) && initial_fd_entry.getCanRead() &&
              !initial_fd_entry.getCanWrite()) &&
            !(initial_fd_entry.getFd() == fileno(stdout) && !initial_fd_entry.getCanRead() &&
              initial_fd_entry.getCanWrite())) {
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
      if (options.dry_run) {
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
              FAIL_IF(pipe2(pipe_fds, O_CLOEXEC)) << "Failed to create pipe: " << ERR;

              file->pipe_reader_fd = pipe_fds[0];
              file->pipe_writer_fd = pipe_fds[1];
            }

            if (initial_fd_entry.getCanRead()) {
              open_fd_ref = &file->pipe_reader_fd;
            } else {  // TODO: check for invalid read/write combinations?
              open_fd_ref = &file->pipe_writer_fd;
            }
          } else {
            int flags = O_CLOEXEC;
            if (initial_fd_entry.getCanRead() && initial_fd_entry.getCanWrite()) {
              flags |= O_RDWR;
            } else if (initial_fd_entry.getCanWrite()) {
              flags |= O_WRONLY;
            } else {  // TODO: what if the database has no permissions for some
                      // reason?
              flags |= O_RDONLY;
            }
            if (file->scheduled_for_creation) {
              file->scheduled_for_creation = false;
              flags |= O_CREAT | O_TRUNC;
            }
            mode_t mode = old_files[initial_fd_entry.getFileID()].getMode();
            open_fd_storage = open(file->path.c_str(), flags, mode);
            FAIL_IF(open_fd_storage == -1) << "Failed to open output: " << ERR;
            open_fd_ref = &open_fd_storage;
          }
          // Ensure that the dup2s won't step on each other's toes
          if (*open_fd_ref <= max_fd) {
            int new_fd = fcntl(*open_fd_ref, F_DUPFD_CLOEXEC, max_fd + 1);
            FAIL_IF(new_fd == -1) << "Failed to remap fd: " << ERR;
            close(*open_fd_ref);
            *open_fd_ref = new_fd;
          }
          if (!file->is_pipe) {
            opened_fds.push_back(*open_fd_ref);
          }
          file_actions.add(
              (InitialFdEntry){.parent_fd = *open_fd_ref, .child_fd = initial_fd_entry.getFd()});
        }
        // Spawn the child
        std::shared_ptr<Command> middle_cmd(
            new Command(run_command->executable, run_command->args));
        child_pid = start_command(graph, middle_cmd, file_actions);
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
    if (options.visualize) {
      rebuild_state.visualize(options.show_sysfiles, options.show_collapsed);
    }
    return 0;
  }

  std::shared_ptr<Command> root_cmd(new Command("Dodofile", {"Dodofile"}));

  graph.setRootCommand(root_cmd);

  // TODO: set up stdio for logging?
  start_command(graph, root_cmd, kj::ArrayPtr<InitialFdEntry const>());

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
        FAIL << "Error while waiting: " << ERR;
      }
    }

    trace_step(graph, child, wait_status);
  }

  Serializer serializer("db.dodo");
  graph.serialize(serializer);
}
