#pragma once

#include <set>
#include <string>

using std::set;
using std::string;

enum class LogLevel { Verbose = 0, Info = 1, Warning = 2, Fatal = 3 };

enum class FingerprintLevel { None, Local, All };

/**
 * Struct to hold command-line options for Dodo, with defaults set here
 */
struct dodo_options {
  set<string> explicitly_changed;
  set<string> explicitly_unchanged;
  bool dry_run = false;
  size_t parallel_jobs = 1;
  bool visualize = false;
  bool show_sysfiles = false;
  bool show_collapsed = true;

  // Fingerprint local or created files by default
  FingerprintLevel fingerprint = FingerprintLevel::Local;

  // Only display log messages that are fatal
  LogLevel log_threshold = LogLevel::Fatal;

  // Color log output by defaulz
  bool color_output = true;

  // Should we display extra debug information on errors?
  bool debug = false;
};

// A single opts struct is defined and populated in driver.cc
extern dodo_options options;
