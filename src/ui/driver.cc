#include <cstdlib>
#include <forward_list>
#include <iostream>
#include <optional>
#include <string>

#include <fcntl.h>
#include <unistd.h>

#include "core/Artifact.hh"
#include "core/BuildGraph.hh"
#include "tracing/Tracer.hh"
#include "ui/Graphviz.hh"
#include "ui/log.hh"
#include "ui/options.hh"
#include "ui/serializer.hh"

using std::cerr;
using std::cout;
using std::endl;
using std::forward_list;
using std::stol;
using std::string;

// Declare the global command-line options struct
dodo_options options;

/**
 * Show command line usage information
 */
void show_usage(string cmd) {
  cout << "Usage: " << cmd << " [options]" << endl;
  cout << "Build Options:" << endl;
  cout << "  -n, --dry-run      Don't run any build commands; just print actions." << endl;
  cout << "  --fingerprint <all|local|none>" << endl;
  cout << "                     Choose which files are fingerprinted to detect changes." << endl;
  cout << "  -j <N>             Allow N jobs to run at once." << endl;
  cout << endl;
  cout << "Debug Output Options:" << endl;
  cout << "  --trace <file>     Write the build trace IR to this file. Pass - for stdout." << endl;
  cout << "  --visualize        Generate graphviz output in file out.dot." << endl;
  cout << "  --visualize-all    Generate graphviz output, including for system files." << endl;
  cout << "  -v                 Print warning-level log information." << endl;
  cout << "  -vv                Print info-level log information." << endl;
  cout << "  -vvv               Print all log information." << endl;
  cout << "  --debug            Include source locations in log messages." << endl;
  cout << "  --no-color         Disable color terminal output." << endl;
}

/**
 * Parse command line options and return a dodo_options struct.
 */
void parse_argv(string cmd, forward_list<string> argv) {
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
          cerr << "Please specifiy a fingerprint level: none, local, or all" << endl;
          show_usage(cmd);
          exit(1);
        }

        argv.pop_front();
      }

    } else if (arg == "-n" || arg == "--dry-run") {
      options.dry_run = true;

    } else if (arg == "-j") {
      if (!argv.empty()) {
        long specified_jobs = stol(argv.front());
        argv.pop_front();

        if (specified_jobs < 1) {
          cerr << "Invalid number of jobs: specify at least one." << endl;
          show_usage(cmd);
          exit(1);
        }
        options.parallel_jobs = specified_jobs;
      } else {
        cerr << "Please specify a number of jobs to use." << endl;
        show_usage(cmd);
        exit(1);
      }

    } else if (arg == "--visualize") {
      options.visualize = true;

    } else if (arg == "--visualize-all") {
      options.visualize = true;
      options.show_sysfiles = true;

    } else if (arg == "--trace") {
      if (!argv.empty()) {
        options.trace_output = argv.front();
        argv.pop_front();
      } else {
        cerr << "Please specify an output file for the build trace." << endl;
      }

    } else if (arg == "-h" || arg == "--help") {
      show_usage(cmd);
      exit(1);
    } else {
      cerr << "Unrecognized argument " << arg << endl;
      show_usage(cmd);
      exit(1);
    }
  }
}

/**
 * Check if the current terminal supports color output.
 */
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
  parse_argv(argv[0], forward_list<string>(argv + 1, argv + argc));

  // Create a build graph to track our build
  BuildGraph graph;

  // Attempt to deserialize the build graph. If that fails, create a new graph
  if (!load_build(".dodo.db", graph)) {
    // We're going to set up a new build graph to run the build. There are three cases to handle:
    //  1. We can just run ./Dodofile
    //  2. Dodofile is not executable. We'll run it with /bin/sh
    //  3. Dodofile is not accessible. This is an error

    if (faccessat(AT_FDCWD, "Dodofile", X_OK, AT_EACCESS) == 0) {
      // Dodofile is directly executable. Initialize graph with a command to run it directly
      graph = BuildGraph("Dodofile", {"Dodofile"});

    } else if (faccessat(AT_FDCWD, "Dodofile", R_OK, AT_EACCESS) == 0) {
      // Dodofile is readable. Initialize graph with a command that runs Dodofile with sh
      graph = BuildGraph("/bin/sh", {"Dodofile", "Dodofile"});

    } else {
      // Dodofile is neither executable nor readable. This won't work.
      FAIL << "Unable to access Dodofile.\n"
           << "  This file must be executable, or a readable file that can be run by /bin/sh.";
    }
  }

  if (!options.dry_run) {
    Tracer tracer;
    graph.run(tracer);
  }

  // Generate graphviz output, if requested
  if (options.visualize) {
    Graphviz g("out.dot");
    graph.drawGraph(g);
  }

  // Generate trace output if requested
  if (options.trace_output.has_value()) {
    string filename = options.trace_output.value();
    if (filename == "-") {
      graph.printTrace(cout);
    } else {
      ofstream f(filename);
      graph.printTrace(f);
    }
  }

  // Serialize the build
  save_build(".dodo.db", graph);

  return 0;
}

template <class Archive>
void serialize(Archive& archive, BuildGraph& g) {
  archive(g._root);
}
