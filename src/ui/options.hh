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

  /****** Optimization ******/
  /// Enable file-staging cache
  inline bool enable_cache = true;  // PAPER

  /// Enable lazy builds (only run commands marked mustRun, come back to the MayRun commands later)
  inline bool lazy_builds = false;  // PAPER

  /****** Debugging ******/
  /// Trace syscall exits to validate outcomes from the filesystem model
  inline bool validate = false;
}