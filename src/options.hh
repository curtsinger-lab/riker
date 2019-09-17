#pragma once

#include "log.hh"

enum log_level : int { Verbose = 0, Info = 1, Warning = 2, Fatal = 3 };

/**
 * Struct to hold command-line options for Dodo, with defaults set here
 */
struct dodo_options {
  bool use_fingerprints = true;
  std::set<std::string> explicitly_changed;
  std::set<std::string> explicitly_unchanged;
  bool dry_run = false;
  size_t parallel_jobs = 1;
  bool visualize = false;
  bool show_sysfiles = false;
  bool show_collapsed = true;
  
  // Only display log messages that are fatal
  log_level log_threshold = log_level::Fatal;
  
  // Do not display source locations with log messages
  bool log_source_locations = false;
};

// A single opts struct is defined and populated in driver.cc
extern dodo_options options;
