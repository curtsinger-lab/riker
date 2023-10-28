#include <cstdio>
#include <filesystem>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include <unistd.h>

#include <CLI/CLI.hpp>

#include "ui/commands.hh"
#include "util/log.hh"
#include "util/options.hh"

namespace fs = std::filesystem;

using std::map;
using std::optional;
using std::set;
using std::string;
using std::vector;

/**
 * Check if the current terminal supports color output.
 */
static bool stderr_supports_colors() noexcept {
  return isatty(STDERR_FILENO) && getenv("TERM") != nullptr;
}

/**
 * This is the entry point for the rkr command line tool
 */
int main(int argc, char* argv[]) noexcept {
  // Set color output based on TERM setting (can be overridden with command line option)
  if (!stderr_supports_colors()) options::disable_color = true;

  // Set up a CLI app for command line parsing
  CLI::App app;

  // We require at least one subcommand
  app.require_subcommand();

  // Option fallthrough allows users to specify global options after a subcommand
  app.fallthrough();

  // Saving flags used into a string for use in remote
  string flags_for_use = "";
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-r") == 0 || strcmp(argv[i], "--remote") == 0) {
      i += 1;
      continue;
    }

    flags_for_use = flags_for_use + argv[i] + " ";
  }

  /************* Global Options *************/
  app.add_flag("--debug", options::debug, "Print source locations with log messages");
  app.add_flag("--no-color", options::disable_color, "Disable color terminal output");

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

  optional<fs::path> stats_log;
  app.add_option("--stats", stats_log,
                 "Path to write statistics to a CSV file; appends if file already exists.")
      ->type_name("FILE");

  app.add_flag_callback("--no-caching", [] { options::enable_cache = false; })
      ->description("Disable the build cache")
      ->group("Optimizations");

  /************* Build Subcommand *************/
  auto build = app.add_subcommand("build", "Perform a build (default)");

  build->add_flag("--show", options::print_on_run, "Show commands as they are run");

  build->add_flag_callback(
      "--show-full",
      [] {
        options::print_on_run = true;
        options::print_full = true;
      },
      "Show complete command lines for all commands as they run");

  build->add_flag_callback(
      "--no-inject", []() { options::inject_tracing_lib = false; },
      "Do not inject the faster shared memory tracing library");

  build->add_flag("--syscall-stats", options::syscall_stats, "Collect system call statistics");

  bool refresh = false;
  build->add_flag("--fresh", refresh, "Run full build");

  // Flags to turn the parallel compiler wrapper on/off
  build
      ->add_flag_callback(
          "--wrapper", []() { options::parallel_wrapper = true; },
          "Wrap C/C++ compilers for parallel separate compilation")
      // Hide the --wrapper flag if it is enabled by default
      ->group(options::parallel_wrapper ? "" : "Options");

  build
      ->add_flag_callback(
          "--no-wrapper", []() { options::parallel_wrapper = false; },
          "Do not wrap C/C++ compilers for parallel separate compilation")
      // Hide the --no-wrapper flag if it is disabled by default
      ->group(options::parallel_wrapper ? "Options" : "");

  string command_output = "-";
  build->add_option("-o,--output", command_output,
                    "Output file where commands should be printed (default: -)");

  string command_binary = "-";
  build->add_option("-b,--bin, --binary", command_binary,
                    "Output file where binary trace should be printed (default: -)");

  // Flags for rkr with remote connections

  optional<string> remote_path;
  build->add_option("-r,--remote", remote_path, "Path to riker on remote system");

  /************* Remote Subcommand *************/
  auto remote = app.add_subcommand("remote", "Perform a remote build (default)");

  remote->add_flag("--show", options::print_on_run, "Show commands as they are run");

  remote->add_flag_callback(
      "--show-full",
      [] {
        options::print_on_run = true;
        options::print_full = true;
      },
      "Show complete command lines for all commands as they run");

  remote->add_flag_callback(
      "--no-inject", []() { options::inject_tracing_lib = false; },
      "Do not inject the faster shared memory tracing library");

  remote->add_flag("--syscall-stats", options::syscall_stats, "Collect system call statistics");

  remote->add_flag("--fresh", refresh, "Run full remote build");

  // Flags to turn the parallel compiler wrapper on/off
  remote
      ->add_flag_callback(
          "--wrapper", []() { options::parallel_wrapper = true; },
          "Wrap C/C++ compilers for parallel separate compilation")
      // Hide the --wrapper flag if it is enabled by default
      ->group(options::parallel_wrapper ? "" : "Options");

  remote
      ->add_flag_callback(
          "--no-wrapper", []() { options::parallel_wrapper = false; },
          "Do not wrap C/C++ compilers for parallel separate compilation")
      // Hide the --no-wrapper flag if it is disabled by default
      ->group(options::parallel_wrapper ? "Options" : "");

  remote->add_option("-o,--output", command_output,
                     "Output file where commands should be printed (default: -)");

  remote->add_option("-b,--bin,--binary", command_binary,
                     "Output file where binary trace should be printed (default: -)");

  // Flags for rkr with remote connections

  remote->add_option("-r,--remote", remote_path, "Path do_remote");

  /************* Audit Subcommand *************/
  auto audit = app.add_subcommand("audit", "Run a full build and print all commands");
  audit->add_option("-o,--output", command_output,
                    "Output file where commands should be printed (default: -)");

  /************* Check Subcommand *************/
  auto check = app.add_subcommand("check", "Check which commands must be rerun");

  /************* Trace Subcommand *************/
  string trace_output = "-";
  string trace_binary = "-";
  string trace_read = "-";

  auto trace = app.add_subcommand("trace", "Print a build trace in human-readable format");
  trace->add_option("-o,--output", trace_output, "Output file for the trace (default: -)");
  trace->add_option("-b, --bin, --binary", trace_binary,
                    "Output file for the binary trace (default: -)");
  trace->add_option("--read", trace_read, "File to manually read binary trace from");

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

  /************* Rikerfile Arguments ***********/
  vector<string> args;
  app.add_option("--args", args, "Arguments to pass to Rikerfile")->group("");  // hidden from help

  /************* Register Callbacks ***********/
  // these are all deferred until the end since a number
  // of them need access to the --args vector
  // Note: using lambdas with reference capture instead of std::bind, since we'd have to wrap
  // every argument in std::ref to pass values by reference.

  // build subcommand
  build->final_callback([&] {
    do_build(args, stats_log, command_output, command_binary, refresh, remote_path, flags_for_use);
  });
  // remote subcommand
  remote->final_callback([&] {
    do_remote(args, stats_log, command_output, command_binary, refresh, remote_path, flags_for_use);
  });
  // audit subcommand
  audit->final_callback([&] { do_audit(args, command_output); });
  // check subcommand
  check->final_callback([&] { do_check(args); });
  // trace subcommand
  trace->final_callback([&] { do_trace(args, trace_output, trace_binary, trace_read); });
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
