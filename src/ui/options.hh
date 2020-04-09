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
  // Only display log messages that are fatal
  LogLevel log_threshold = LogLevel::Fatal;

  // Do not print color codes with log output
  bool disable_color = false;

  // Should we display extra debug information on errors?
  bool debug = false;
};

// A single opts struct is defined and populated in driver.cc
extern dodo_options options;
