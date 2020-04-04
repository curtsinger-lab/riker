#pragma once

#include <optional>
#include <set>
#include <string>

using std::optional;
using std::set;
using std::string;

enum class LogLevel { Verbose = 0, Info = 1, Warning = 2, Fatal = 3 };

enum class FingerprintLevel { None, Local, All };

/**
 * Struct to hold command-line options for Dodo, with defaults set here
 */
struct dodo_options {
  // Should the build be faked with a dry run instead of a real build?
  bool dry_run = false;

  // How many jobs should be run in parallel at one time?
  size_t parallel_jobs = 1;

  // Fingerprint only local and temporary files by default. Use mtime for others.
  FingerprintLevel fingerprint = FingerprintLevel::Local;

  // Only display log messages that are fatal
  LogLevel log_threshold = LogLevel::Fatal;

  // Color log output by defaulz
  bool color_output = true;

  // Should we display extra debug information on errors?
  bool debug = false;

  // Should we generate graphviz output?
  bool visualize = false;

  // Should graphviz output include system files?
  bool show_sysfiles = false;

  // Should we generate a trace of command operations?
  optional<string> trace_output;
};

// A single opts struct is defined and populated in driver.cc
extern dodo_options options;
