#include <cstdlib>
#include <filesystem>
#include <forward_list>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <string>

#include <fcntl.h>
#include <unistd.h>

#include <CLI/CLI.hpp>

#include "core/Artifact.hh"
#include "core/Build.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"
#include "util/GraphVisitor.hh"
#include "util/StatsVisitor.hh"
#include "util/TraceVisitor.hh"
#include "util/serializer.hh"

using std::cerr;
using std::cout;
using std::endl;
using std::forward_list;
using std::make_unique;
using std::ofstream;
using std::stol;
using std::string;

namespace fs = std::filesystem;

// Constant names for files and paths
const fs::path RootBuildCommand = "Dodofile";
const fs::path OutputDir = ".dodo";
const fs::path DatabaseFilename = ".dodo/db";

void do_build(bool dry_run, int jobs, string fingerprint) {
  // Create a build object
  Build b;
  bool loaded = false;
  try {
    b = load_build(DatabaseFilename);
    loaded = true;
  } catch (db_version_exception e) {
    WARN << "Build database is outdated. Running a clean build.";
  } catch (cereal::Exception e) {
    // Silently fall back to a clean build when there is no build database file
  }

  // Attempt to load a saved build. If that fails, create a new build object
  if (!loaded) {
    // We're going to set up a new build graph to run the build. There are three cases to handle:
    //  1. We can just run ./Dodofile
    //  2. Dodofile is not executable. We'll run it with /bin/sh
    //  3. Dodofile is not accessible. This is an error

    if (faccessat(AT_FDCWD, RootBuildCommand.c_str(), X_OK, AT_EACCESS) == 0) {
      // Dodofile is directly executable. Initialize graph with a command to run it directly
      b = Build(RootBuildCommand, {RootBuildCommand});

    } else if (faccessat(AT_FDCWD, RootBuildCommand.c_str(), R_OK, AT_EACCESS) == 0) {
      // Dodofile is readable. Initialize graph with a command that runs Dodofile with sh
      b = Build("/bin/sh", {RootBuildCommand, RootBuildCommand});

    } else {
      // Dodofile is neither executable nor readable. This won't work.
      FAIL << "Unable to access " << RootBuildCommand << ".\n"
           << "  This file must be executable, or a readable file that can be run by /bin/sh.";
    }
  }

  if (!dry_run) {
    Tracer tracer;
    b.run(tracer);
  }

  // Make sure the output directory exists
  fs::create_directories(OutputDir);

  // Serialize the build
  save_build(DatabaseFilename, b);
}

void do_check() {
  try {
    Build b = load_build(DatabaseFilename);
    b.check();

  } catch (db_version_exception e) {
    FAIL << "Build database is outdated. Rerun the build to create a new build database.";
  } catch (cereal::Exception e) {
    FAIL << "Failed to load the build database. Have you run a build yet?";
  }
}

void do_trace(string output) {
  try {
    Build b = load_build(DatabaseFilename);

    if (output == "-") {
      cout << TraceVisitor(b);
    } else {
      ofstream f(output);
      f << TraceVisitor(b);
    }
  } catch (db_version_exception e) {
    FAIL << "Build database is outdated. Rerun the build to create a new build database.";
  } catch (cereal::Exception e) {
    FAIL << "Failed to load the build database. Have you run a build yet?";
  }
}

void do_graph(string output, bool show_sysfiles) {
  try {
    Build b = load_build(DatabaseFilename);

    if (output == "-") {
      cout << GraphVisitor(b, show_sysfiles);
    } else {
      ofstream f(output);
      f << GraphVisitor(b, show_sysfiles);
    }

  } catch (db_version_exception e) {
    FAIL << "Build database is outdated. Rerun the build to create a new build database.";
  } catch (cereal::Exception e) {
    FAIL << "Failed to load the build database. Have you run a build yet?";
  }
}

void do_stats(bool list_artifacts) {
  try {
    Build b = load_build(DatabaseFilename);
    cout << StatsVisitor(b, list_artifacts);

  } catch (db_version_exception e) {
    FAIL << "Build database is outdated. Rerun the build to create a new build database.";
  } catch (cereal::Exception e) {
    FAIL << "Failed to load the build database. Have you run a build yet?";
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
  if (!stderr_supports_colors()) logger::disable_color = true;

  // Set up a CLI app for command line parsing
  CLI::App app;

  // We require at least one subcommand
  app.require_subcommand();

  // Option fallthrough allows users to specify global options after a subcommand
  app.fallthrough();

  /************* Global Options *************/
  app.add_flag("--debug", logger::debug, "Print source locations with log messages");
  app.add_flag("--no-color", logger::disable_color, "Disable color terminal output");
  app.add_flag(
      "-v",
      [](int count) {
        if (count == 0) {
          logger::log_level = LogLevel::Fatal;
        } else if (count == 1) {
          logger::log_level = LogLevel::Warning;
        } else if (count == 2) {
          logger::log_level = LogLevel::Info;
        } else {
          logger::log_level = LogLevel::Verbose;
        }
      },
      "Increase logging verbosity.");

  /************* Build Subcommand *************/
  bool dry_run = false;
  int jobs = 0;
  string fingerprint = "local";

  auto build = app.add_subcommand("build", "Perform a build (default)");

  build->add_flag("-n,--dry-run", dry_run, "Do not run any build commands");

  build->add_option("-j,--jobs", jobs, "Max concurrent jobs (0 is unlimited)")
      ->check(CLI::NonNegativeNumber);

  build->add_option("-f,--fingerprint", fingerprint, "Set the fingerprint level (default=local)")
      ->transform(CLI::IsMember({"all", "local", "none"}, CLI::ignore_case));

  build->add_flag_callback("--no-ignore-self-reads", [] { Build::ignore_self_reads = false; })
      ->description("Disable the ignore-self-reads optimization")
      ->group("Optimizations");

  build->add_flag_callback("--no-write-combine", [] { Build::combine_writes = false; })
      ->description("Disable the write-combining optimization")
      ->group("Optimizations");

  build->add_flag_callback("--no-skip-repeat-checks", [] { Build::skip_repeat_checks = false; })
      ->description("Disable the repeat check skipping optimization")
      ->group("Optimizations");

  // Set the callback for the build subcommand
  // Note: using a lambda with reference capture instead of std::bind, since we'd have to wrap
  // every argument in std::ref to pass values by reference.
  build->final_callback([&] { do_build(dry_run, jobs, fingerprint); });

  /************* Check Subcommand *************/
  auto check = app.add_subcommand("check", "Check which commands must be rerun and report why");
  check->final_callback([&] { do_check(); });

  /************* Trace Subcommand *************/
  string trace_output = "-";

  auto trace = app.add_subcommand("trace", "Print a build trace in human-readable format");
  trace->add_option("-o,--output", trace_output, "Output file for the trace (default: -)");

  // Set the callback fo the trace subcommand
  trace->final_callback([&] { do_trace(trace_output); });

  /************* Graph Subcommand *************/
  string graph_output = "out.dot";
  bool show_sysfiles = false;

  auto graph = app.add_subcommand("graph", "Generate a build graph");
  graph->add_option("-o,--output", graph_output, "Output file for the graph (default: out.dot)");
  graph->add_flag("-a,--all", show_sysfiles, "Include system files in graph output");

  // Set the callback fo the trace subcommand
  graph->final_callback([&] { do_graph(graph_output, show_sysfiles); });

  /************* Stats Subcommand *************/
  bool list_artifacts = false;

  auto stats = app.add_subcommand("stats", "Print build statistics");
  stats->add_flag("-a,--artifacts", list_artifacts, "Print a list of artifacts and their versions");

  stats->final_callback([&] { do_stats(list_artifacts); });

  /************* Argument Parsing *************/

  try {
    // Try to parse the arguments as-is
    app.parse(argc, argv);
  } catch (const CLI::CallForHelp& e) {
    // When the options requested help, just print it and exit
    return app.exit(e);
  } catch (const CLI::ParseError& e) {
    // If the option parse failed, we may need to retry with the build subcommand set by default
    // Only do this if we did NOT receive a subcommand already
    if (app.get_subcommands().size() == 0) {
      // Reset the command line parse
      app.clear();

      // Build a new argv with the default subcommand inserted
      vector<const char*> new_argv;
      new_argv.push_back(argv[0]);
      new_argv.push_back("build");
      for (int i = 1; i < argc; i++) {
        new_argv.push_back(argv[i]);
      }

      try {
        // We can pass a vector to CLI11, but it expects the values to be reversed
        // Instead, pass the count and data pointers from our new_argv vector
        app.parse(new_argv.size(), new_argv.data());
      } catch (const CLI::ParseError& e) {
        return app.exit(e);
      }
    } else {
      // An error occurred with a subcommand specified. Handle the error in the normal way.
      return app.exit(e);
    }
  }

  return 0;
}
