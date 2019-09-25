#pragma once

#include <set>
#include <string>

#include "ui/log.hh"

enum class LogLevel { Verbose = 0, Info = 1, Warning = 2, Fatal = 3 };

enum class FingerprintLevel { None, Local, All };

/**
 * Struct to hold command-line options for Dodo, with defaults set here
 */
struct dodo_options {
  std::set<std::string> explicitly_changed;
  std::set<std::string> explicitly_unchanged;
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

  // Do not display source locations with log messages
  bool log_source_locations = false;
};

// A single opts struct is defined and populated in driver.cc
extern dodo_options options;
