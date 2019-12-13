#include <cstdlib>
#include <forward_list>
#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <unistd.h>

#include "core/Artifact.hh"
#include "core/BuildGraph.hh"
#include "tracing/Tracer.hh"
#include "ui/Graphviz.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::cerr;
using std::cout;
using std::endl;
using std::forward_list;
using std::shared_ptr;
using std::stol;
using std::string;
using std::vector;

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
      options.debug = true;
      options.log_threshold = LogLevel::Info;

    } else if (arg == "--no-color") {
      options.color_output = false;

    } else if (arg == "-v") {
      options.log_threshold = LogLevel::Warning;

    } else if (arg == "-vv") {
      options.log_threshold = LogLevel::Info;

    } else if (arg == "-vvv") {
      options.log_threshold = LogLevel::Verbose;

    } else if (arg == "--fingerprint") {
      if (!argv.empty()) {
        if (argv.front() == "none") {
          options.fingerprint = FingerprintLevel::None;
        } else if (argv.front() == "local") {
          options.fingerprint = FingerprintLevel::Local;
        } else if (argv.front() == "all") {
          options.fingerprint = FingerprintLevel::All;
        } else {
          FAIL << "Please specifiy a fingerprint level: none, local, or all.";
        }

        argv.pop_front();
      }

    } else if (arg == "--changed") {
      if (!argv.empty()) {
        options.explicitly_changed.insert(argv.front());
        argv.pop_front();
      } else {
        cerr << "Please specify a file to mark as changed.\n";
        exit(1);
      }

    } else if (arg == "--unchanged") {
      if (!argv.empty()) {
        options.explicitly_unchanged.insert(argv.front());
        argv.pop_front();
      } else {
        cerr << "Please specify a file to mark as unchanged.\n";
        exit(1);
      }

    } else if (arg == "--dry-run") {
      options.dry_run = true;

    } else if (arg == "-j") {
      if (!argv.empty()) {
        long specified_jobs = stol(argv.front());
        argv.pop_front();

        if (specified_jobs < 1) {
          cerr << "Invalid number of jobs: specify at least one.\n";
          exit(1);
        }
        options.parallel_jobs = specified_jobs;
      } else {
        cerr << "Please specify a number of jobs to use" << endl;
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
      cerr << "Invalid argument " << arg << endl;
      exit(1);
    }
  }
}

static bool stderr_supports_colors() {
  return isatty(STDERR_FILENO) && getenv("TERM") != nullptr;
}

/**
 * This is the entry point for the dodo command line tool
 */
int main(int argc, char* argv[]) {
  // Set color output based on TERM setting (can be overridden with command line option)
  if (!stderr_supports_colors()) options.color_output = false;

  // Parse command line options
  parse_argv(forward_list<string>(argv + 1, argv + argc));

  // Create a build graph to track our build
  BuildGraph graph;

  // Attempt to deserialize the build graph. If that fails, create a new graph
  if (!graph.load("db.dodo")) {
    graph = BuildGraph("Dodofile");
  }

  Tracer tracer(graph);
  graph.run(tracer);

  // Run the standard graph post-processing to prune cycles
  // Currently just drops inputs that are also outputs from the same command
  graph.prune();

  // Generate graphviz output, if requested
  if (options.visualize) {
    Graphviz g("out.dot");
    graph.drawGraph(g);
  }

  return 0;
}
