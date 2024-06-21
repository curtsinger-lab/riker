#pragma once

#include <chrono>
#include <cstddef>
#include <filesystem>
#include <optional>
#include <string>

namespace fs = std::filesystem;

namespace stats {
  /// The time set when the stats counters were last reset
  inline std::chrono::time_point start_time = std::chrono::high_resolution_clock::now();

  /// The number of unique commands that have been emulated
  inline size_t emulated_commands = 0;

  /// The number of unique commands that have been traced
  inline size_t traced_commands = 0;

  /// The number of emulated IR steps
  inline size_t emulated_steps = 0;

  /// The number of traced IR steps
  inline size_t traced_steps = 0;

  /// The total number of artifacts
  inline size_t artifacts = 0;

  /// The total number of versions
  inline size_t versions = 0;

  /// The total number of ptrace stops
  inline size_t ptrace_stops = 0;

  /// The total number of traced syscalls
  inline size_t syscalls = 0;
}

/// Reset all stats counters to their default values
inline static void reset_stats() noexcept {
  stats::start_time = std::chrono::high_resolution_clock::now();
  stats::emulated_commands = 0;
  stats::traced_commands = 0;
  stats::emulated_steps = 0;
  stats::traced_steps = 0;
  stats::artifacts = 0;
  stats::versions = 0;
  stats::ptrace_stops = 0;
  stats::syscalls = 0;
}
