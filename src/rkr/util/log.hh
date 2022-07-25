#pragma once

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>

#include <experimental/source_location>

#include <fcntl.h>
#include <sys/resource.h>
#include <sys/time.h>

#include <fmt/core.h>
#include <fmt/ostream.h>

#include "util/options.hh"
#include "util/terminate.hh"

#define NORMAL "\033[00;"
#define BOLD "\033[01;"
#define FAINT "\033[02;"

#define GREEN "32m"
#define BLUE "34m"
#define YELLOW "33m"
#define RED "31m"
#define WHITE "38m"

#define END_COLOR "\033[01;0m"

/// Specify log categories, and indicate each with a distinct bit
enum class LogCategory { error, warning, trace, ir, artifact, rebuild, exec, phase, cache };

constexpr const char* getLogCategoryName(LogCategory category) {
  if (category == LogCategory::error) return "error";
  if (category == LogCategory::warning) return "warning";
  if (category == LogCategory::trace) return "trace";
  if (category == LogCategory::ir) return "ir";
  if (category == LogCategory::artifact) return "artifact";
  if (category == LogCategory::rebuild) return "rebuild";
  if (category == LogCategory::exec) return "exec";
  if (category == LogCategory::phase) return "phase";
  if (category == LogCategory::cache) return "cache";
  return "unknown";
}

// Printing unique_ptr values is allowed
template <class T>
static std::ostream& operator<<(std::ostream& o, const std::unique_ptr<T>& p) {
  return o << p.get();
}

// Format shared pointers by dereference
template <typename T>
struct fmt::formatter<std::shared_ptr<T>> : fmt::formatter<T> {
  template <typename FormatContext>
  auto format(std::shared_ptr<T> p, FormatContext& ctx) const {
    if (p) {
      return fmt::formatter<T>::format(*p, ctx);
    } else {
      return fmt::format_to(ctx.out(), "null");
    }
  }
};

/**
 * This class is used for logging to the console. The macros defined below return an instance of
 * this class, which they can then print through using the << operator. The logger will
 * automatically insert a newline at the end of the output. If instantiated with exit=true, the
 * logger aborts the program when output is finished.
 */
template <LogCategory category>
class logger {
 public:
  /// Is this logger enabled? By default, error and warning are enabled, while others are disabled.
  inline static bool enabled = (category == LogCategory::error || category == LogCategory::warning);

  /// What is the name of this logger?
  inline static constexpr const char* name = getLogCategoryName(category);

 public:
  logger(std::experimental::source_location location =
             std::experimental::source_location::current()) noexcept {
    // Stop immediately if this logger is not enabled
    if (!enabled) return;

    // Set the color for the log category text
    if (!options::disable_color) {
      if (category == LogCategory::error) {
        std::cerr << FAINT RED;
      } else if (category == LogCategory::warning) {
        std::cerr << FAINT YELLOW;
      } else {
        std::cerr << FAINT GREEN;
      }
    }

    // Print the logging category name
    std::cerr << "(" << name << ") ";

    // Print source information, if enabled
    if (options::debug) {
      if (!options::disable_color) std::cerr << NORMAL BLUE;
      std::cerr << "[" << location.file_name() << ":" << location.line() << "] ";
    }

    // Set the log color for the actual message
    if (!options::disable_color) {
      if (category == LogCategory::error) {
        std::cerr << NORMAL RED;
      } else if (category == LogCategory::warning) {
        std::cerr << NORMAL YELLOW;
      } else {
        std::cerr << NORMAL GREEN;
      }
    }
  }

  ~logger() noexcept {
    if (!enabled) return;

    // End color output and print a newline
    if (!options::disable_color) std::cerr << END_COLOR;
    std::cerr << "\n";
    std::cerr << std::dec;

    // If this log is a fatal call the terminate handler
    if (category == LogCategory::error) terminate();
  }

  template <typename T>
  logger& operator<<(const T& t) noexcept {
    if (enabled) std::cerr << t;
    return *this;
  }
};

/// A null logger never prints anything, but can serve as a stand-in for a logger when logging is
/// disabled (e.g. by turning off assert checking)
class null_logger {
 public:
  template <class T>
  null_logger& operator<<(const T& t) noexcept {
    return *this;
  }
};

#define LOG(type) \
  if (logger<LogCategory::type>::enabled) logger<LogCategory::type>()

#define LOGF(type, format_str, ...) LOG(type) << fmt::format(format_str, __VA_ARGS__)

// Define shorthand macros for specific log types
#define WARN LOG(warning)
#define FAIL LOG(error)

// Define conditional warning
#define WARN_IF(cond) \
  if (cond) WARN

// Define conditional failure macros
#define FAIL_IF(cond) \
  if (cond) FAIL
#define FAIL_UNLESS(cond) \
  if (!(cond)) FAIL

// Define a shortcut for printing the error message corresponding to the current errno
#define ERR strerror(errno)

// NDEBUG-controlled ASSERT macro
#ifdef NDEBUG
#define ASSERT(cond) null_logger()
#else
#define ASSERT(cond) FAIL_UNLESS(cond)
#endif
