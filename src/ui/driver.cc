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

/// Run the `build` subcommand.
void do_build() noexcept {
  // Load a trace, or set up a default build if necessary
  auto trace = load_trace(DatabaseFilename, true);

  // Set up a build to emulate the loaded command tree.
  Build phase1;

  // Set up a rebuild planner to observe the emulated build
  auto rebuild = make_shared<RebuildPlanner>();
  phase1.addObserver(rebuild);

  // Run the emulated build to gather change and dependency information
  phase1.run(trace);

  // Now create a build to run the second phase, the actual build execution
  // Pass in the print_on_run and dry_run options this time
  Build phase2;

  // Prepare the build to execute the necessary commands
  rebuild->planBuild(phase2);

  // Execute the planned build
  auto [final_trace, final_env] = phase2.run(trace);

  // Commit the final state of the build to the filesystem and take fingerprints
  final_env->commitFinalState();

  // Make sure the output directory exists
  fs::create_directories(OutputDir);

  // Serialize the build trace
  save_trace(DatabaseFilename, final_trace);
}

/**
 * Run the `check` subcommand
 */
void do_check() noexcept {
  // Load a build, or set up a default build if necessary
  auto trace = load_trace(DatabaseFilename, true);

  // Set up a build to emulate the loaded command tryy
  Build phase1;

  // Set up a rebuild planner to observe the emulated build
  auto rebuild = make_shared<RebuildPlanner>();
  phase1.addObserver(rebuild);

  // Run the emulated build to gather change and dependency information
  phase1.run(trace);

  // Print the rebuild planning dependency information
  cout << rebuild;

  // Plan a build
  Build phase2;
  rebuild->planBuild(phase2);

  // Print the planned build
  cout << phase2;
}

/**
 * Run the `trace` subcommand
 * \param output  The name of the output file, or "-" for stdout
 */
void do_trace(string output) noexcept {
  // Load the build trace
  auto trace = load_trace(DatabaseFilename, false);

  // Print it
  if (output == "-") {
    cout << trace;
  } else {
    ofstream f(output);
    f << trace;
  }
}

/**
 * Run the `graph` subcommand
 * \param fingerprint The configuration the build should use for fingerprinting artifacts
 * \param output      The name of the output file, or "-" for stdout
 * \param type        The type of output to produce
 * \param show_all    If true, include system files in the graph
 * \param no_render   If set, generate graphviz source instead of a rendered graph
 */
void do_graph(string output, string type, bool show_all, bool no_render) noexcept {
  if (type.empty() && no_render) type = "dot";
  if (type.empty() && !no_render) type = "png";
  if (output.empty()) output = "out." + type;

  // If the output filename is not empty, but has no extension, append one
  if (output.find('.') == string::npos) output += "." + type;

  // Load the command tree
  auto trace = load_trace(DatabaseFilename, false);

  // Set up a build to emulate the command tree
  Build b;

  // Create the Graph observer and attach it to the build
  auto graph = make_shared<Graph>(show_all);
  b.addObserver(graph);

  // Run the emulated build
  b.run(trace);

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
 * \param fingerprint     The configuration the build should use for fingerprinting artifacts
 * \param list_artifacts  Should the output include a list of artifacts and versions?
 */
void do_stats(bool list_artifacts) noexcept {
  // Load the serialized command tree
  auto trace = load_trace(DatabaseFilename, false);

  // Set up a build to emulate the commands
  Build b;

  // Emulate the build
  auto [final_trace, final_env] = b.run(trace);

  // Count the number of versions of artifacts
  size_t version_count = 0;
  for (const auto& artifact : final_env->getArtifacts()) {
    version_count += artifact->getVersionCount();
  }

  // Print statistics
  cout << "Build Statistics:" << endl;
  cout << "  Commands: " << trace->getCommands().size() << endl;
  cout << "  Steps: " << trace->getSteps().size() << endl;
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

  app.add_option_function<LogCategory>(
         "--log", [&](LogCategory opt) { logger::log_categories |= (1 << (int)opt); },
         "Display log messages from one or more additional categories")
      ->delimiter(',')
      ->take_all()
      ->type_name("CATEGORY")
      ->transform(
          CLI::CheckedTransformer(map<string, LogCategory>{{"warning", LogCategory::warning},
                                                           {"syscall", LogCategory::syscall},
                                                           {"ir", LogCategory::ir},
                                                           {"artifact", LogCategory::artifact},
                                                           {"rebuild", LogCategory::rebuild},
                                                           {"exec", LogCategory::exec}},
                                  CLI::ignore_case)
              .description("{warning, syscall, ir, artifact, rebuild, exec}"));

  app.add_option("--fingerprint", options::fingerprint_level,
                 "Set the fingerprint level (default=local)")
      ->type_name("LEVEL")
      ->transform(
          CLI::CheckedTransformer(map<string, FingerprintLevel>{{"all", FingerprintLevel::All},
                                                                {"local", FingerprintLevel::Local},
                                                                {"none", FingerprintLevel::None}},
                                  CLI::ignore_case)
              .description("{all, local, none}"));

  app.add_flag_callback("--no-caching", [] { options::enable_cache = false; })
      ->description("Disable the build cache")
      ->group("Optimizations");

  /************* Build Subcommand *************/
  auto build = app.add_subcommand("build", "Perform a build (default)");

  build->add_flag("--show", options::print_on_run, "Show commands as they are run");

  build->add_flag("-n,--dry-run", options::dry_run, "Do not run any build commands");

  // Set the callback for the build subcommand
  // Note: using a lambda with reference capture instead of std::bind, since we'd have to wrap
  // every argument in std::ref to pass values by reference.
  build->final_callback([&] { do_build(); });

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
  bool show_all = false;
  bool no_render = false;

  auto graph = app.add_subcommand("graph", "Generate a build graph");
  graph->add_option("-o,--output", graph_output, "Output file for the graph");
  graph->add_option("-t,--type", graph_type, "Output format for the graph (png, pdf, jpg, etc.)");
  graph->add_flag("-n,--no-render", no_render, "Generate graphiz source instead of rendering");
  graph->add_flag("-a,--all", show_all, "Include all files in the graph");

  // Set the callback fo the trace subcommand
  graph->final_callback([&] { do_graph(graph_output, graph_type, show_all, no_render); });

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
