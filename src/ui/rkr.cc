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

  app.add_flag("--mtime-only", options::mtime_only, "When fingerprinting, only rely on mtime.");

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

  build->add_flag("-n,--dry-run", options::dry_run, "Do not run any build commands");

  build
      ->add_flag_callback(
          "--eager", []() { options::lazy_builds = false; },
          "Eagerly run all commands that may need to run during a rebuild")
      // Hide the --eager flag if eager builds are enabled by default
      ->group(options::lazy_builds ? "Options" : "");

  build
      ->add_flag("--lazy", options::lazy_builds,
                 "Lazily run commands only as they are needed during rebuild")
      // Hide the --lazy flag if lazy builds are enabled by default
      ->group(options::lazy_builds ? "" : "Options");

  build->add_flag("--inject", options::inject_tracing_lib,
                  "Inject a shared library for shared memory tracing");

  string command_output = "-";
  build->add_option("-o,--output", command_output,
                    "Output file where commands should be printed (default: -)");

  /************* Audit Subcommand *************/
  auto audit = app.add_subcommand("audit", "Run a full build and print all commands");

  audit->add_option("-o,--output", command_output,
                    "Output file where commands should be printed (default: -)");

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

  /************* Dependency Subcommand ********/
  bool create_container = false;
  bool create_snap_squashfs = false;
  bool create_snap_snapcraft = false;
  auto gen_deps = app.add_subcommand(
      "generate-deps", "Generate .rkr-deps that contains all the necessary dependencies");
  auto install_deps = app.add_subcommand("install-deps", "Install all missing dependencies");
  auto check_deps = app.add_subcommand("check-deps", "Check all the necessary dependencies");
  gen_deps->add_flag(
      "-c,--container", create_container,
      "Generate a Dockerfile in the .devcontainer folder that is compatible with vscode");
  gen_deps->add_flag(
      "-q, --snap-squashfs", create_snap_squashfs,
      "Generate a Snap using squashfs");
  gen_deps->add_flag(
      "-s, --snap-snapcraft", create_snap_snapcraft,
      "Generate a Snap using snapcraft");
  // auto gen_container = app.add_subcommand(
  //     "create-container",
  //     "Generate a Dockerfile in the .devcontainer folder that is compatible with vscode");

  /*********** Clean subcommand ****************/
  bool clean_all = false;
  auto clean =
      app.add_subcommand("clean", "Remove cached information to do a fresh build of the program");
  clean->add_flag("-a,--all", clean_all, "Remove all the files generated during a build");

  /************* Rikerfile Arguments ***********/
  vector<string> args;
  app.add_option("--args", args, "Arguments to pass to Rikerfile")->group("");  // hidden from help

  /************* Register Callbacks ***********/
  // these are all deferred until the end since a number
  // of them need access to the --args vector
  // Note: using lambdas with reference capture instead of std::bind, since we'd have to wrap
  // every argument in std::ref to pass values by reference.

  // build subcommand
  build->final_callback([&] { do_build(args, stats_log, command_output); });
  // audit subcommand
  audit->final_callback([&] { do_audit(args, command_output); });
  // check subcommand
  check->final_callback([&] { do_check(args); });
  // trace subcommand
  trace->final_callback([&] { do_trace(args, trace_output); });
  // graph subcommand
  graph->final_callback([&] { do_graph(args, graph_output, graph_type, show_all, no_render); });
  // stats subcommand
  stats->final_callback([&] { do_stats(args, list_artifacts); });
  // dependency subcommand
  gen_deps->final_callback([&] { do_gen_deps(args, create_container, create_snap_squashfs, create_snap_snapcraft); });
  install_deps->final_callback([&] { do_install_deps(args); });
  check_deps->final_callback([&] { do_check_deps(args); });
  // gen_container->final_callback([&] { do_gen_container(args); });
  // cleaning subcommand
  clean->final_callback([&] { do_clean(args, clean_all); });

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
