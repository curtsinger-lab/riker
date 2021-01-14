#pragma once

enum class FingerprintLevel { None, Local, All };

// Namespace to contain global flags that control build behavior
namespace options {
  // The length limit for commands printed to the terminal
  enum { command_length = 80 };

  /// Print commands as they are executed
  inline bool print_on_run = false;

  /// Print commands instead of running them
  inline bool dry_run = false;

  /// Which files, if any, should be fingerprinted?
  inline FingerprintLevel fingerprint_level = FingerprintLevel::Local;

  /// When fingerprinting, do we use a hash function (i.e., BLAKE3)?
  inline bool mtime_only = false;

  /// Should commands keep a precise record of their inputs and outputs? Used for graph.
  inline bool track_inputs_outputs = false;

  /// When set, disable color terminal output
  inline bool disable_color = false;

  /// When set, include source locations in log messages
  inline bool debug = false;

  /****** Optimization ******/
  /// Enable file-staging cache
  inline bool enable_cache = true;  // PAPER

  /// Run only commands that MUST run, then re-evaluate and execute others as needed
  inline bool lazy_builds = true;  // PAPER

}