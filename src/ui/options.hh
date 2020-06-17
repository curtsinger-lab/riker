#pragma once

enum class FingerprintLevel { None, Local, All };

// Namespace to contain global flags that control build behavior
namespace options {
  /// Should the build print commands as they are run?
  inline bool print_on_run = false;

  /// Is this a dry run?
  inline bool dry_run = false;

  /// Which files should be fingerprinted instead of just checked for mtime?
  inline FingerprintLevel fingerprint_level = FingerprintLevel::Local;

  /// The maximum length of a command when printed
  inline size_t command_length = 80;

  /****** Optimization ******/
  /// Repeated reads can be combined into a single read
  inline bool combine_reads = true;  // PAPER

  /// Repeated writes with no interleaved read can be combined into a single write
  inline bool combine_writes = true;  // PAPER

  /// Skip repeated checks of the contents or metadata for the same reference
  inline bool skip_repeat_checks = true;  // PAPER

  /// Enable file-staging cache
  inline bool enable_cache = true;  // PAPER
}