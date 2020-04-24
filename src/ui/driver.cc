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

/// Get a default build
Build get_default_build() {
  // We're going to set up a new build graph to run the build. There are three cases to handle:
  //  1. We can just run ./Dodofile
  //  2. Dodofile is not executable. We'll run it with /bin/sh
  //  3. Dodofile is not accessible. This is an error

  if (faccessat(AT_FDCWD, RootBuildCommand.c_str(), X_OK, AT_EACCESS) == 0) {
    // Dodofile is directly executable. Initialize graph with a command to run it directly
    return Build(RootBuildCommand, {RootBuildCommand});

  } else if (faccessat(AT_FDCWD, RootBuildCommand.c_str(), R_OK, AT_EACCESS) == 0) {
    // Dodofile is readable. Initialize graph with a command that runs Dodofile with sh
    return Build("/bin/sh", {RootBuildCommand, RootBuildCommand});

  } else {
    // Dodofile is neither executable nor readable. This won't work.
    FAIL << "Unable to access " << RootBuildCommand << ".\n"
         << "  This file must be executable, or a readable file that can be run by /bin/sh.";
  }
  return Build();  // Unreachable, but silences warnings
}

/// Attempt to load a serialized build, but fall back to a default build on failure
Build load_build_or_default() {
  try {
    return load_build(DatabaseFilename);
  } catch (db_version_exception e) {
    WARN << "Build database is outdated. Resetting to a default, clean build.";
  } catch (cereal::Exception e) {
    // Silently fall back to a clean build when there is no build database file
  }

  return get_default_build();
}

/// Try to load a build. Exit with an error if loading fails.
Build require_load_build() {
  try {
    return load_build(DatabaseFilename);
  } catch (db_version_exception e) {
    FAIL << "Build database is outdated. Rerun the build to create a new build database.";
  } catch (cereal::Exception e) {
    FAIL << "Failed to load the build database. Have you run a build yet?";
  }
  return Build();  // Unreachable, but silences warnings
}

/**
 * Run the `build` subcommand.
 * \param dry_run     If set, commands are not run but will instead be printed to the terminal
 * \param jobs        The maximum number of concurrent jobs to run
 * \param fingerprint The type of fingerprinting to use, "none", "local", or "all".
 *                    The value is checked by the command line parsing code.
 */
void do_build(bool dry_run, int jobs, string fingerprint) {
  // Load a build, or set up a default build if necessary
  Build b = load_build_or_default();

  if (!dry_run) {
    Tracer tracer;
    b.run(tracer);
  }

  // Make sure the output directory exists
  fs::create_directories(OutputDir);

  // Serialize the build
  save_build(DatabaseFilename, b);
}

/**
 * Run the `check` subcommand
 */
void do_check(bool default_build) {
  if (default_build) {
    get_default_build().check();
  } else {
    Build b = require_load_build();
    b.check();
  }
}

/**
 * Run the `trace` subcommand
 * \param output  The name of the output file, or "-" for stdout
 */
void do_trace(string output) {
  Build b = require_load_build();
  if (output == "-") {
    cout << TraceVisitor(b);
  } else {
    ofstream f(output);
    f << TraceVisitor(b);
  }
}

/**
 * Run the `graph` subcommand
 * \param output        The name of the output file, or "-" for stdout
 * \param show_sysfiles If true, include system files in the graph
 */
void do_graph(string output, bool show_sysfiles) {
  Build b = require_load_build();
  if (output == "-") {
    cout << GraphVisitor(b, show_sysfiles);
  } else {
    ofstream f(output);
    f << GraphVisitor(b, show_sysfiles);
  }
}

/**
 * Run the `stats` subcommand
 * \param list_artifacts  Should the output include a list of artifacts and versions?
 */
void do_stats(bool list_artifacts) {
  Build b = require_load_build();
  cout << StatsVisitor(b, list_artifacts);
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
  bool default_build = false;

  auto check = app.add_subcommand("check", "Check which commands must be rerun and report why");

  check->add_flag("-d,--default", default_build, "Check the state of a default build");

  check->final_callback([&] { do_check(default_build); });

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
