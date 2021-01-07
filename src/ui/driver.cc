#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include <unistd.h>

#include <CLI/CLI.hpp>

#include "artifacts/Artifact.hh"
#include "data/IRBuffer.hh"
#include "data/IRSource.hh"
#include "data/InputTrace.hh"
#include "data/OutputTrace.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/PostBuildChecker.hh"
#include "runtime/env.hh"
#include "ui/Graph.hh"
#include "ui/TracePrinter.hh"
#include "ui/constants.hh"
#include "ui/options.hh"
#include "ui/stats.hh"
#include "util/log.hh"

using std::cout;
using std::endl;
using std::make_unique;
using std::map;
using std::ofstream;
using std::optional;
using std::set;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

namespace fs = std::filesystem;

/**
 * Run the `build` subcommand.
 */
void do_build(vector<string> args, optional<fs::path> stats_log_path, bool print_header) noexcept {
  // If the user asked for a header, just print it out and quit
  if (print_header) {
    write_empty_stats(stats_log_path);
    exit(EXIT_SUCCESS);
  }

  // Make sure the output directory exists
  fs::create_directories(constants::OutputDir);

  // Also ensure that the cache directory exists
  fs::create_directory(constants::CacheDir);

  // Build stats
  optional<string> stats;

  // Reset the statistics counters
  reset_stats();

  // Load a trace, or set up a default build if necessary
  // Trace is lazy-loaded, so work is not done here
  unique_ptr<IRSource> input = InputTrace::load(constants::DatabaseFilename, args);

  size_t iteration = 0;
  bool done = false;
  bool trace_changed = false;
  while (!done) {
    // Create a buffer to hold the IR output
    auto output = make_unique<IRBuffer>();

    LOGF(phase, "Starting build phase {}", iteration);

    // Reset the environment
    env::reset();

    // Run the build
    Build build(true, *output);
    input->sendTo(build);

    LOGF(phase, "Finished build phase {}", iteration);

    // Check if any commands must run on the next iteration. If so, we are not done.
    done = true;
    for (auto& c : build.getCommands()) {
      if (c->mustRerun()) {
        done = false;
        trace_changed = true;
      }
    }

    // If this is iteration 0, report the end of phase 1
    if (iteration == 0) {
      gather_stats(stats_log_path, stats, "pre");
      reset_stats();
    }

    // If we're done, commit all changes
    if (done) {
      LOGF(phase, "Committing environment changes from build phase {}", iteration);
      env::commitAll();
    }

    // The output becomes the next iteration's input
    input = std::move(output);
    iteration++;
  }

  gather_stats(stats_log_path, stats, "rebuild");
  reset_stats();

  // If any commands had to rerun, run post-build checks and write out a new trace
  if (trace_changed) {
    LOG(phase) << "Starting post-build checks";

    // Run the post-build checks
    IRBuffer post_build_buffer;
    PostBuildChecker post_build_chcker(post_build_buffer);

    // Reset the environment
    env::reset();

    // Run the build
    Build build(false, post_build_chcker);
    input->sendTo(build);

    LOG(phase) << "Finished post-build checks";

    // Write the final trace to disk
    OutputTrace output(constants::DatabaseFilename);
    post_build_buffer.sendTo(output);
  }

  gather_stats(stats_log_path, stats, "post");
  write_stats(stats_log_path, stats);
}

/**
 * Run the `check` subcommand
 */
void do_check(vector<string> args) noexcept {
  // Load a build, or set up a default build if necessary
  auto trace = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the loaded trace
  Build build(false);
  trace->sendTo(build);

  // Print commands that must run
  bool must_run_header_printed = false;
  for (const auto& c : build.getCommands()) {
    if (c->getMarking() == RebuildMarking::MustRun) {
      // Print the header if necessary
      if (!must_run_header_printed) {
        cout << "Commands that must run:" << endl;
        must_run_header_printed = true;
      }

      // Print the command
      cout << "  " << c->getShortName(options::command_length) << endl;
    }
  }

  // If we printed anything, add a newline
  if (must_run_header_printed) cout << endl;

  // Print the rebuild plan
  bool may_run_header_printed = false;
  for (const auto& c : build.getCommands()) {
    if (c->getMarking() == RebuildMarking::MayRun) {
      if (!may_run_header_printed) {
        cout << "Commands that may run:" << endl;
        may_run_header_printed = true;
      }
      cout << "  " << c->getShortName(options::command_length) << endl;
    }
  }

  // If we never printed the header, there were no commands to rerun
  if (!must_run_header_printed && !may_run_header_printed) {
    cout << "No commands to rerun" << endl;
  }
}

/**
 * Run the `trace` subcommand
 * \param output    The name of the output file, or "-" for stdout
 */
void do_trace(vector<string> args, string output) noexcept {
  // Are we printing to stdout or a file?
  if (output == "-") {
    InputTrace::load(constants::DatabaseFilename, args)->sendTo(TracePrinter(cout));
  } else {
    InputTrace::load(constants::DatabaseFilename, args)->sendTo(TracePrinter(ofstream(output)));
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
  auto trace = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the build
  Build build(false);
  trace->sendTo(build);

  Graph graph(show_all);
  graph.addCommands(build.getCommands());

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
  // Reset the stats counters
  reset_stats();

  // Load the serialized build trace
  auto trace = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  Build build(false);
  trace->sendTo(build);

  // Print statistics
  cout << "Build Statistics:" << endl;
  cout << "  Commands: " << stats::emulated_commands << endl;
  cout << "  Steps: " << stats::emulated_steps << endl;
  cout << "  Artifacts: " << stats::artifacts << endl;
  cout << "  Artifact Versions: " << stats::versions << endl;

  if (list_artifacts) {
    cout << endl;
    cout << "Artifacts:" << endl;
    for (const auto& a : env::getArtifacts()) {
      if (a->getName().empty()) {
        cout << "  " << a->getTypeName() << ": <anonymous>" << endl;
      } else {
        cout << "  " << a->getTypeName() << ": " << a->getName() << endl;
      }

      size_t index = 0;
      for (const auto& v : a->getMetadataVersions()) {
        cout << "    mv" << index << ": " << v << endl;
        index++;
      }
      index = 0;
      for (const auto& v : a->getContentVersions()) {
        cout << "    cv" << index << ": " << v << endl;
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
             if (category == "phase" || category == "all") {
               logger<LogCategory::phase>::enabled = true;
             }
             if (category == "cache" || category == "all") {
               logger<LogCategory::cache>::enabled = true;
             }
           }
         },
         "Display log messages from one or more categories")
      ->type_name("CATEGORY")
      ->transform(
          CLI::IsMember(
              {"warning", "trace", "ir", "artifact", "rebuild", "exec", "phase", "cache", "all"},
              CLI::ignore_case)
              .description("{warning, trace, ir, artifact, rebuild, exec, phase, cache, all}"))
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

  app.add_flag("--mtime-only", options::mtime_only, "When fingerprinting, only rely on mtime.");

  std::optional<std::filesystem::path> stats_log;
  app.add_option("--stats", stats_log,
                 "Path to write statistics to a CSV file; appends if file already exists.")
      ->type_name("FILE");
  bool print_header = false;
  app.add_flag(
      "--empty-stats", print_header,
      "Write an empty stats CSV and quit; requires --stats but all other options are ignored.");

  app.add_flag_callback("--no-caching", [] { options::enable_cache = false; })
      ->description("Disable the build cache")
      ->group("Optimizations");

  /************* Build Subcommand *************/
  auto build = app.add_subcommand("build", "Perform a build (default)");

  build->add_flag("--show", options::print_on_run, "Show commands as they are run");

  build->add_flag("-n,--dry-run", options::dry_run, "Do not run any build commands");

  // TODO: Swap this for an --eager flag once lazy builds are the default
  build->add_flag("--lazy", options::lazy_builds, "Run commands as needed during the build");

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
  build->final_callback([&] { do_build(args, stats_log, print_header); });
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
