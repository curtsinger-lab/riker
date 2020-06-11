#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include <unistd.h>

#include <CLI/CLI.hpp>

#include "build/Build.hh"
#include "observers/Graph.hh"
#include "observers/RebuildPlanner.hh"
#include "observers/Stats.hh"
#include "observers/Trace.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::cout;
using std::endl;
using std::make_shared;
using std::ofstream;
using std::string;
using std::stringstream;
using std::vector;

namespace fs = std::filesystem;

// Constant names for files and paths
const char* RootBuildCommand = "Dodofile";
const char* ShellCommand = "/bin/sh";
const fs::path OutputDir = ".dodo";
const fs::path DatabaseFilename = ".dodo/db";

/**
 * Run the `build` subcommand.
 * \param jobs        The maximum number of concurrent jobs to run
 * \param fingerprint The type of fingerprinting to use, "none", "local", or "all".
 *                    The value is checked by the command line parsing code.
 */
void do_build(int jobs) {
  // Load a build, or set up a default build if necessary
  auto root = load_build(DatabaseFilename, true);

  // Set up a build to emulate the loaded command tree
  Build phase1(root);

  // Set up a rebuild planner to observe the emulated build
  auto rebuild = make_shared<RebuildPlanner>();
  phase1.addObserver(rebuild);

  // Run the emulated build to gather change and dependency information
  phase1.run();

  // Now create a build to run the second phase, the actual build execution
  Build phase2(root);

  // Prepare the build to execute the necessary commands
  rebuild->planBuild(phase2);

  // Execute the planned build
  phase2.run();

  // Make sure the output directory exists
  fs::create_directories(OutputDir);

  // Serialize the build
  save_build(DatabaseFilename, root);
}

/**
 * Run the `check` subcommand
 */
void do_check() {
  // Load a build, or set up a default build if necessary
  auto root = load_build(DatabaseFilename, true);

  // Set up a build to emulate the loaded command tryy
  Build phase1(root);

  // Set up a rebuild planner to observe the emulated build
  auto rebuild = make_shared<RebuildPlanner>();
  phase1.addObserver(rebuild);

  // Run the emulated build to gather change and dependency information
  phase1.run();

  // Print the rebuild planning dependency information
  cout << rebuild;

  // Plan a build
  Build phase2(root);
  rebuild->planBuild(phase2);

  // Print the planned build
  cout << phase2;
}

/**
 * Run the `trace` subcommand
 * \param output  The name of the output file, or "-" for stdout
 */
void do_trace(string output) {
  // Load the root command
  auto root = load_build(DatabaseFilename, false);

  // Set up a build to emulate the command tree
  Build b(root);

  // Set the trace printer as an observer on the build
  auto trace = make_shared<Trace>();
  b.addObserver(trace);

  // Run the emulated build
  b.run();

  if (output == "-") {
    cout << trace;
  } else {
    ofstream f(output);
    f << trace;
  }
}

/**
 * Run the `graph` subcommand
 * \param output        The name of the output file, or "-" for stdout
 * \param show_sysfiles If true, include system files in the graph
 */
void do_graph(string output, string type, bool show_sysfiles, bool no_render) {
  if (type.empty() && no_render) type = "dot";
  if (type.empty() && !no_render) type = "png";
  if (output.empty()) output = "out." + type;

  // If the output filename is not empty, but has no extension, append one
  if (output.find('.') == string::npos) output += "." + type;

  // Load the command tree
  auto root = load_build(DatabaseFilename, false);

  // Set up a build to emulate the command tree
  Build b(root);

  // Create the Graph observer and attach it to the build
  auto graph = make_shared<Graph>(show_sysfiles);
  b.addObserver(graph);

  // Run the emulated build
  b.run();

  if (no_render) {
    ofstream f(output);
    f << graph;

  } else {
    // Send graph source to a stringstream
    stringstream ss;
    ss << graph;

    // Write the graph output to a temporary file, then move that file to stdin
    auto tmp = std::tmpfile();
    std::fputs(ss.str().c_str(), tmp);
    std::rewind(tmp);
    dup2(fileno(tmp), STDIN_FILENO);

    // Exec dot
    execlp("dot", "dot", "-T", type.c_str(), "-o", output.c_str(), NULL);

    FAIL << "Failed to render graph with dot. Is graphviz installed?";
  }
}

/**
 * Run the `stats` subcommand
 * \param list_artifacts  Should the output include a list of artifacts and versions?
 */
void do_stats(bool list_artifacts) {
  // Load the serialized command tree
  auto root = load_build(DatabaseFilename, false);

  // Set up a build to emulate the commands
  Build b(root);

  // Create a stats observer and attach it to the build
  auto stats = make_shared<Stats>(list_artifacts);
  b.addObserver(stats);

  // Emulate the build
  b.run();

  // Print the result
  cout << stats;
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

  app.add_option_function<string>(
         "--fingerprint",
         [](string opt) {
           if (opt == "all") {
             options::fingerprint_level = FingerprintLevel::All;
           } else if (opt == "local") {
             options::fingerprint_level = FingerprintLevel::Local;
           } else {
             options::fingerprint_level = FingerprintLevel::None;
           }
         },
         "Set the fingerprint level (default=local)")
      ->transform(CLI::IsMember({"all", "local", "none"}, CLI::ignore_case));

  app.add_flag_callback("--no-read-combine", [] { options::combine_reads = false; })
      ->description("Disable the logic to combine repeated reads in the command trace")
      ->group("Optimizations");

  app.add_flag_callback("--no-write-combine", [] { options::combine_writes = false; })
      ->description("Disable the logic to combine repeated writes in the command trace")
      ->group("Optimizations");

  app.add_flag_callback("--no-skip-repeat-checks", [] { options::skip_repeat_checks = false; })
      ->description("Disable the repeat check skipping optimization")
      ->group("Optimizations");

  app.add_flag_callback("--no-caching", [] { options::enable_cache = false; })
      ->description("Disable the build cache")
      ->group("Optimizations");

  /************* Build Subcommand *************/
  int jobs = 0;

  auto build = app.add_subcommand("build", "Perform a build (default)");

  build->add_flag("--show", options::print_on_run, "Show commands as they are run");

  build->add_flag("-n,--dry-run", options::dry_run, "Do not run any build commands");

  build->add_option("-j,--jobs", jobs, "Max concurrent jobs (0 is unlimited)")
      ->check(CLI::NonNegativeNumber);

  // Set the callback for the build subcommand
  // Note: using a lambda with reference capture instead of std::bind, since we'd have to wrap
  // every argument in std::ref to pass values by reference.
  build->final_callback([&] { do_build(jobs); });

  /************* Check Subcommand *************/
  auto check = app.add_subcommand("check", "Check which commands must be rerun");

  check->final_callback([&] { do_check(); });

  /************* Trace Subcommand *************/
  string trace_output = "-";

  auto trace = app.add_subcommand("trace", "Print a build trace in human-readable format");
  trace->add_option("-o,--output", trace_output, "Output file for the trace (default: -)");

  // Set the callback fo the trace subcommand
  trace->final_callback([&] { do_trace(trace_output); });

  /************* Graph Subcommand *************/
  // Leave output file and type empty for later default processing
  string graph_output;
  string graph_type;
  bool show_sysfiles = false;
  bool no_render = false;

  auto graph = app.add_subcommand("graph", "Generate a build graph");
  graph->add_option("-o,--output", graph_output, "Output file for the graph");
  graph->add_option("-t,--type", graph_type, "Output format for the graph (png, pdf, jpg, etc.)");
  graph->add_flag("-n,--no-render", no_render, "Generate graphiz source instead of rendering");
  graph->add_flag("-a,--all", show_sysfiles, "Include system files in graph output");

  // Set the callback fo the trace subcommand
  graph->final_callback([&] { do_graph(graph_output, graph_type, show_sysfiles, no_render); });

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
