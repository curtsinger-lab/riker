#pragma once

enum class FingerprintLevel { None, Local, All };

// Namespace to contain global flags that control build behavior
namespace options {
  // The length limit for commands printed to the terminal
  enum { command_length = 80 };

  /// Print commands as they are executed
  inline bool print_on_run = false;

  /// Should full commands be printed instead of abbreviations?
  inline bool print_full = false;

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

  /// When set, gather system call stats and report them at the end of a build
  inline bool syscall_stats = false;

  /****** Optimization ******/
  /// Enable file-staging cache
  inline bool enable_cache = true;  // PAPER

  /// Run only commands that MUST run, then re-evaluate and execute others as needed
  inline bool lazy_builds = true;  // PAPER

  /// The maximum file size to consider for caching
  // inline size_t max_cached_file_size = 1024 * 1024 * 10;

  /// Inject the shared memory tracing library
  inline bool inject_tracing_lib = false;

  /// Use the parallel compiler wrapper
  inline bool parallel_wrapper = false;
}