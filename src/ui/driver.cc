#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

#include <sys/wait.h>
#include <unistd.h>

#include <CLI/CLI.hpp>

#include "data/InputTrace.hh"
#include "data/OutputTrace.hh"
#include "observers/Graph.hh"
#include "observers/RebuildPlanner.hh"
#include "runtime/Build.hh"
#include "runtime/PostBuildChecker.hh"
#include "ui/TracePrinter.hh"
#include "ui/options.hh"
#include "ui/stats.hh"
#include "util/log.hh"

using std::cout;
using std::endl;
using std::ofstream;
using std::optional;
using std::string;
using std::stringstream;
using std::vector;

namespace fs = std::filesystem;

// Constant names for files and paths
const char* RootBuildCommand = "Dodofile";
const char* ShellCommand = "/bin/sh";
const fs::path OutputDir = ".dodo";
const fs::path DatabaseFilename = ".dodo/db";
const fs::path NewDatabaseFilename = ".dodo/newdb";

/**
 * Run the `build` subcommand.
 */
void do_build(vector<string> args, optional<fs::path> stats_log_path) noexcept {
  // Make sure the output directory exists
  fs::create_directories(OutputDir);

  // Load a trace, or set up a default build if necessary
  InputTrace trace(args, DatabaseFilename);

  // Set up a rebuild planner to observe the emulated build
  RebuildPlanner planner;

  // Build stats
  optional<string> stats;

  {
    // Emulate the loaded trace
    auto build = Build::emulate(planner);
    trace.sendTo(build);

    // Save rebuild stats
    gather_stats(stats_log_path, build, stats, "pre");
  }

  {
    // Set up an output trace
    OutputTrace output(NewDatabaseFilename);

    // Now run the trace again with the planned rebuild steps
    auto build = Build::rebuild(planner.planBuild(), output);
    trace.sendTo(build);

    // Save rebuild stats
    gather_stats(stats_log_path, build, stats, "rebuild");
  }

  {
    // Load the newly generated input trace
    InputTrace new_trace(args, NewDatabaseFilename);

    // Set up an output trace for the post-build check
    OutputTrace output(DatabaseFilename);

    // Set up a filter to update predicates to their post-build state
    PostBuildChecker filter(output);

    // Emulate the new trace
    auto build = Build::emulate(filter);
    new_trace.sendTo(build);

    // Save rebuild stats
    gather_stats(stats_log_path, build, stats, "post");
  }

  // write stats
  write_stats(stats_log_path, stats);
}

/**
 * Run the `check` subcommand
 */
void do_check(vector<string> args) noexcept {
  // Load a build, or set up a default build if necessary
  InputTrace trace(args, DatabaseFilename);

  // Set up a rebuild planner to observe the emulated build
  RebuildPlanner planner;

  // Emulate the loaded trace
  trace.sendTo(Build::emulate(planner));

  // Print commands whose inputs have changed
  if (planner.getChanged().size() > 0) {
    cout << "Commands with changed inputs:" << endl;
    for (const auto& c : planner.getChanged()) {
      cout << "  " << c->getShortName(options::command_length) << endl;
    }
    cout << endl;
  }

  // Print commands whose output is needed
  if (planner.getOutputNeeded().size() > 0) {
    cout << "Commands whose output is missing or modified:" << endl;
    for (const auto& c : planner.getOutputNeeded()) {
      cout << "  " << c->getShortName(options::command_length) << endl;
    }
    cout << endl;
  }

  // Print the rebuild plan
  cout << planner.planBuild();
}

/**
 * Run the `trace` subcommand
 * \param output    The name of the output file, or "-" for stdout
 */
void do_trace(vector<string> args, string output) noexcept {
  // Are we printing to stdout or a file?
  if (output == "-") {
    InputTrace(args, DatabaseFilename).sendTo(TracePrinter(cout));
  } else {
    InputTrace(args, DatabaseFilename).sendTo(TracePrinter(ofstream(output)));
  }
}

/**
 * Run the `graph` subcommand
 * \param output      The name of the output file, or "-" for stdout
 * \param type        The type of output to produce
 * \param show_all    If true, include system files in the graph
 * \param no_render   If set, generate graphviz source instead of a rendered graph
 */
void do_graph(vector<string> args,
              string output,
              string type,
              bool show_all,
              bool no_render) noexcept {
  if (type.empty() && no_render) type = "dot";
  if (type.empty() && !no_render) type = "png";
  if (output.empty()) output = "out." + type;

  // If the output filename is not empty, but has no extension, append one
  if (output.find('.') == string::npos) output += "." + type;

  // Load the build trace
  InputTrace trace(args, DatabaseFilename);

  // Create a graph observer and emulate the build
  Graph graph(show_all);
  trace.sendTo(Build::emulate(graph));

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
void do_stats(vector<string> args, bool list_artifacts) noexcept {
  // Load the serialized build trace
  InputTrace trace(args, DatabaseFilename);

  // Emulate the trace
  auto build = Build::emulate();
  trace.sendTo(build);
  auto final_env = build.getEnvironment();

  // Count the number of versions of artifacts
  size_t version_count = 0;
  for (const auto& artifact : final_env->getArtifacts()) {
    version_count += artifact->getVersionCount();
  }

  // Print statistics
  cout << "Build Statistics:" << endl;
  cout << "  Commands: " << build.getCommandCount() << endl;
  cout << "  Steps: " << build.getStepCount() << endl;
  cout << "  Artifacts: " << final_env->getArtifacts().size() << endl;
  cout << "  Artifact Versions: " << version_count << endl;

  if (list_artifacts) {
    cout << endl;
    cout << "Artifacts:" << endl;
    for (const auto& a : final_env->getArtifacts()) {
      if (a->getName().empty()) {
        cout << "  " << a->getTypeName() << ": <anonymous>" << endl;
      } else {
        cout << "  " << a->getTypeName() << ": " << a->getName() << endl;
      }

      size_t index = 0;
      for (const auto& v : a->getVersions()) {
        cout << "    v" << index << ": " << v << endl;
        index++;
      }
      cout << endl;
    }
  }
}

/**
 * Check if the current terminal supports color output.
 */
static bool stderr_supports_colors() noexcept {
  return isatty(STDERR_FILENO) && getenv("TERM") != nullptr;
}

/**
 * This is the entry point for the dodo command line tool
 */
int main(int argc, char* argv[]) noexcept {
  // Set color output based on TERM setting (can be overridden with command line option)
  if (!stderr_supports_colors()) logger_options::disable_color = true;

  // Set up a CLI app for command line parsing
  CLI::App app;

  // We require at least one subcommand
  app.require_subcommand();

  // Option fallthrough allows users to specify global options after a subcommand
  app.fallthrough();

  /************* Global Options *************/
  app.add_flag("--debug", logger_options::debug, "Print source locations with log messages");
  app.add_flag("--no-color", logger_options::disable_color, "Disable color terminal output");

  app.add_option_function<set<string>>(
         "--log",
         [&](set<string> categories) {
           for (auto category : categories) {
             if (category == "warning" || category == "all") {
               logger<LogCategory::warning>::enabled = true;
             }
             if (category == "trace" || category == "all") {
               logger<LogCategory::trace>::enabled = true;
             }
             if (category == "ir" || category == "all") {
               logger<LogCategory::ir>::enabled = true;
             }
             if (category == "artifact" || category == "all") {
               logger<LogCategory::artifact>::enabled = true;
             }
             if (category == "rebuild" || category == "all") {
               logger<LogCategory::rebuild>::enabled = true;
             }
             if (category == "exec" || category == "all") {
               logger<LogCategory::exec>::enabled = true;
             }
           }
         },
         "Display log messages from one or more categories")
      ->type_name("CATEGORY")
      ->transform(CLI::IsMember({"warning", "trace", "ir", "artifact", "rebuild", "exec", "all"},
                                CLI::ignore_case)
                      .description("{warning, trace, ir, artifact, rebuild, exec, all}"))
      ->delimiter(',');
  app.add_option("--fingerprint", options::fingerprint_level,
                 "Set the fingerprint level (default=local)")
      ->type_name("LEVEL")
      ->transform(
          CLI::CheckedTransformer(map<string, FingerprintLevel>{{"all", FingerprintLevel::All},
                                                                {"local", FingerprintLevel::Local},
                                                                {"none", FingerprintLevel::None}},
                                  CLI::ignore_case)
              .description("{all, local, none}"));

  std::optional<std::filesystem::path> stats_log;
  app.add_option("--stats", stats_log,
                 "Path to write statistics to a CSV file; appends if file already exists.")
      ->type_name("FILE");

  app.add_flag_callback("--no-caching", [] { options::enable_cache = false; })
      ->description("Disable the build cache")
      ->group("Optimizations");

  /************* Build Subcommand *************/
  auto build = app.add_subcommand("build", "Perform a build (default)");

  build->add_flag("--show", options::print_on_run, "Show commands as they are run");

  build->add_flag("-n,--dry-run", options::dry_run, "Do not run any build commands");

  /************* Check Subcommand *************/
  auto check = app.add_subcommand("check", "Check which commands must be rerun");

  /************* Trace Subcommand *************/
  string trace_output = "-";

  auto trace = app.add_subcommand("trace", "Print a build trace in human-readable format");
  trace->add_option("-o,--output", trace_output, "Output file for the trace (default: -)");

  /************* Graph Subcommand *************/
  // Leave output file and type empty for later default processing
  string graph_output;
  string graph_type;
  bool show_all = false;
  bool no_render = false;

  auto graph = app.add_subcommand("graph", "Generate a build graph");
  graph->add_option("-o,--output", graph_output, "Output file for the graph");
  graph->add_option("-t,--type", graph_type, "Output format for the graph (png, pdf, jpg, etc.)");
  graph->add_flag("-n,--no-render", no_render, "Generate graphiz source instead of rendering");
  graph->add_flag("-a,--all", show_all, "Include all files in the graph");

  /************* Stats Subcommand *************/
  bool list_artifacts = false;

  auto stats = app.add_subcommand("stats", "Print build statistics");
  stats->add_flag("-a,--artifacts", list_artifacts, "Print a list of artifacts and their versions");

  /************* Dodofile Arguments ***********/
  std::vector<string> args;
  app.add_option("--args", args, "Arguments to pass to Dodofile")->group("");  // hidden from help

  /************* Register Callbacks ***********/
  // these are all deferred until the end since a number
  // of them need access to the --args vector
  // Note: using lambdas with reference capture instead of std::bind, since we'd have to wrap
  // every argument in std::ref to pass values by reference.

  // build subcommand
  build->final_callback([&] { do_build(args, stats_log); });
  // check subcommand
  check->final_callback([&] { do_check(args); });
  // trace subcommand
  trace->final_callback([&] { do_trace(args, trace_output); });
  // graph subcommand
  graph->final_callback([&] { do_graph(args, graph_output, graph_type, show_all, no_render); });
  // stats subcommand
  stats->final_callback([&] { do_stats(args, list_artifacts); });

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
