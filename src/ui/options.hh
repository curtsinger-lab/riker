#pragma once

#include <filesystem>

namespace fs = std::filesystem;

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

  /// Where are cached files saved?
  inline fs::path cache_dir = ".dodo/cache";
}