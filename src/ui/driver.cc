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
 * \param buildfile The path to the file that should be executed at the root of the build
 */
void do_build(fs::path buildfile) noexcept {
  // Make sure the output directory exists
  fs::create_directories(OutputDir);

  // Load a trace, or set up a default build if necessary
  InputTrace trace(buildfile, DatabaseFilename);

  // Set up a rebuild planner to observe the emulated build
  RebuildPlanner planner;

  // Emulate the loaded trace
  trace.sendTo(Build::emulate(planner));

  {
    // Set up an output trace
    OutputTrace output(NewDatabaseFilename);

    // Now run the trace again with the planned rebuild steps
    trace.sendTo(Build::rebuild(planner.planBuild(), output));
  }

  {
    // Load the newly generated input trace
    InputTrace new_trace(buildfile, NewDatabaseFilename);

    // Set up an output trace for the post-build check
    OutputTrace output(DatabaseFilename);

    // Set up a filter to update predicates to their post-build state
    PostBuildChecker filter(output);

    // Emulate the new trace
    new_trace.sendTo(Build::emulate(filter));
  }
}

/**
 * Run the `check` subcommand
 * \param buildfile The path to the file that should be executed at the root of the build
 */
void do_check(fs::path buildfile) noexcept {
  // Load a build, or set up a default build if necessary
  InputTrace trace(buildfile, DatabaseFilename);

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
 * \param buildfile The path to the file that should be executed at the root of the build
 * \param output    The name of the output file, or "-" for stdout
 */
void do_trace(fs::path buildfile, string output) noexcept {
  // Are we printing to stdout or a file?
  if (output == "-") {
    InputTrace(buildfile, DatabaseFilename).sendTo(TracePrinter(cout));
  } else {
    InputTrace(buildfile, DatabaseFilename).sendTo(TracePrinter(ofstream(output)));
  }
}

/**
 * Run the `graph` subcommand
 * \param buildfile   The path to the file that should be executed at the root of the build
 * \param output      The name of the output file, or "-" for stdout
 * \param type        The type of output to produce
 * \param show_all    If true, include system files in the graph
 * \param no_render   If set, generate graphviz source instead of a rendered graph
 */
void do_graph(fs::path buildfile,
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
  InputTrace trace(buildfile, DatabaseFilename);

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
 * \param buildfile       The path to the file that should be executed at the root of the build
 * \param list_artifacts  Should the output include a list of artifacts and versions?
 */
void do_stats(fs::path buildfile, bool list_artifacts) noexcept {
  // Load the serialized build trace
  InputTrace trace(buildfile, DatabaseFilename);

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
 * Import build steps from another build system
 * \returns The path to the generated build file
 */
fs::path import_build() {
  // Create a directory to store the imported build steps
  fs::create_directories(OutputDir);

  // Look for a Makefile
  optional<fs::path> makefile_path;
  if (fs::exists("GNUmakefile")) {
    makefile_path = "GNUmakefile";
  } else if (fs::exists("makefile")) {
    makefile_path = "makefile";
  } else if (fs::exists("Makefile")) {
    makefile_path = "Makefile";
  }

  // Did we find a makefile?
  if (makefile_path.has_value()) {
    // Make a path to the imported makefile steps
    fs::path output = fs::path(OutputDir) / "Makefile-steps";

    // If the imported list of steps does not exist, or if it's older than the makefile, import
    if (!fs::exists(output) ||
        fs::last_write_time(makefile_path.value()) > fs::last_write_time(output)) {
      LOG(exec) << "Importing build steps from " << makefile_path.value();

      int output_fd = open(output.c_str(), O_CREAT | O_TRUNC | O_WRONLY, 0640);
      FAIL_IF(output_fd < 0) << "Failed to create file " << output << ": " << ERR;

      // Fork a child to run `make`
      auto child_pid = fork();
      FAIL_IF(child_pid == -1) << "Fork failed: " << ERR;

      if (child_pid == 0) {
        // In the child

        // Remap the output file to stdout
        int rc = dup2(output_fd, STDOUT_FILENO);
        FAIL_IF(rc != STDOUT_FILENO) << "Failed to direct make output to " << output << ": " << ERR;

        // Remap stdin and stderr to /dev/null
        int null_fd = open("/dev/null", O_RDWR);
        FAIL_IF(rc < 0) << "Failed to open /dev/null: " << ERR;

        rc = dup2(null_fd, STDIN_FILENO);
        FAIL_IF(rc != STDIN_FILENO) << "Failed to redirect make stdin to /dev/null: " << ERR;

        rc = dup2(null_fd, STDERR_FILENO);
        FAIL_IF(rc != STDERR_FILENO) << "Failed to redirect make stderr to /dev/null: " << ERR;

        // Launch make
        execlp("make", "make", "--always-make", "-n", "--quiet", NULL);

        FAIL << "Failed to execute make: " << ERR;

      } else {
        // In the parent
        int status;
        auto rc = waitpid(child_pid, &status, 0);
        FAIL_IF(rc != child_pid) << "Failed to wait for make: " << ERR;

        if (WIFSIGNALED(status) || WEXITSTATUS(status) != 0) {
          FAIL << "Import from make failed";
        }

        // Close the output file
        close(output_fd);
      }

      // The output file now holds the steps we're going to run. Return it
      return output;
    }
  }

  // Fall back to the root build command. This file probably does not exist (that's why import_build
  // was called) but will cause dodo-launch to generate a reasonable error message.
  return RootBuildCommand;
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

  // Look for a build file. If there isn't one, try to import a build from an existing build system
  fs::path buildfile = RootBuildCommand;
  if (!fs::exists(buildfile)) {
    buildfile = import_build();
  }

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
  build->final_callback([&] { do_build(buildfile); });

  /************* Check Subcommand *************/
  auto check = app.add_subcommand("check", "Check which commands must be rerun");

  check->final_callback([&] { do_check(buildfile); });

  /************* Trace Subcommand *************/
  string trace_output = "-";

  auto trace = app.add_subcommand("trace", "Print a build trace in human-readable format");
  trace->add_option("-o,--output", trace_output, "Output file for the trace (default: -)");

  // Set the callback fo the trace subcommand
  trace->final_callback([&] { do_trace(buildfile, trace_output); });

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
  graph->final_callback(
      [&] { do_graph(buildfile, graph_output, graph_type, show_all, no_render); });

  /************* Stats Subcommand *************/
  bool list_artifacts = false;

  auto stats = app.add_subcommand("stats", "Print build statistics");
  stats->add_flag("-a,--artifacts", list_artifacts, "Print a list of artifacts and their versions");

  stats->final_callback([&] { do_stats(buildfile, list_artifacts); });

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
