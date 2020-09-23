#pragma once

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <utility>

#include <experimental/source_location>

#include <fcntl.h>
#include <fmt/core.h>
#include <fmt/ostream.h>

using std::cerr;
using std::move;
using std::experimental::source_location;

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
enum class LogCategory : int {
  error = 1,
  warning = 2,
  trace = 4,
  ir = 8,
  artifact = 16,
  rebuild = 32,
  exec = 64
};

constexpr const char* getLogCategoryName(LogCategory category) {
  if (category == LogCategory::error) return "error";
  if (category == LogCategory::warning) return "warning";
  if (category == LogCategory::trace) return "trace";
  if (category == LogCategory::ir) return "ir";
  if (category == LogCategory::artifact) return "artifact";
  if (category == LogCategory::rebuild) return "rebuild";
  if (category == LogCategory::exec) return "exec";
  return "unknown";
}

// Printing unique_ptr values is allowed
template <class T>
static std::ostream& operator<<(std::ostream& o, const std::unique_ptr<T>& p) {
  return o << p.get();
}

struct logger_options {
  /// When set, disable color terminal output
  inline static bool disable_color = false;

  /// When set, include source locations in log messages
  inline static bool debug = false;
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

  inline static constexpr const char* name = getLogCategoryName(category);

 private:
  bool _enabled;  // Should this logger print anything?
  bool _done;     // Is this the final command in the log output?

 public:
  logger(source_location location = source_location::current()) noexcept : _done(true) {
    // Stop immediately if this logger is not enabled
    if (!enabled) return;

    // Set the color for the log category text
    if (!logger_options::disable_color) {
      if (category == LogCategory::error) {
        cerr << FAINT RED;
      } else if (category == LogCategory::warning) {
        cerr << FAINT YELLOW;
      } else {
        cerr << FAINT GREEN;
      }
    }

    // Print the logging category name
    cerr << "(" << name << ") ";

    // Print source information, if enabled
    if (logger_options::debug) {
      if (!logger_options::disable_color) cerr << NORMAL BLUE;
      cerr << "[" << location.file_name() << ":" << location.line() << "] ";
    }

    // Set the log color for the actual message
    if (!logger_options::disable_color) {
      if (category == LogCategory::error) {
        cerr << NORMAL RED;
      } else if (category == LogCategory::warning) {
        cerr << NORMAL YELLOW;
      } else {
        cerr << NORMAL GREEN;
      }
    }
  }

  logger(logger&& other) noexcept {
    _done = other._done;
    other._done = false;
  }

  ~logger() noexcept {
    if (!enabled) return;

    if (_done) {
      // End color output and print a newline
      if (!logger_options::disable_color) cerr << END_COLOR;
      cerr << "\n";
      cerr << std::dec;

      // If this log is a fatal
      if (category == LogCategory::error) {
        // In debug mode, call abort() so we can run a backtrace. Otherwise exit with failure.
        if (logger_options::debug)
          abort();
        else
          exit(2);
      }
    }
  }

  void operator=(logger&& other) noexcept {
    _done = other._done;
    other._done = false;
  }

  template <typename T>
  logger&& operator<<(const T& t) noexcept {
    if (enabled) cerr << t;
    return move(*this);
  }

  using StreamType = decltype(std::cerr);
  using EndlType = StreamType& (*)(StreamType&);

  logger&& operator<<(EndlType e) { return move(*this); }
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
