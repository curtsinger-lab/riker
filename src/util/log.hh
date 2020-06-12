#pragma once

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <utility>

using std::cerr;
using std::move;

#define COLOR_VERBOSE "\033[00;32m"
#define COLOR_INFO "\033[01;34m"
#define COLOR_WARNING "\033[01;33m"
#define COLOR_FATAL "\033[01;31m"
#define COLOR_SOURCE "\033[34m"
#define COLOR_END "\033[0m"

enum class LogLevel { Fatal = -1, Warning = 0, Info = 1, Verbose = 2 };

/**
 * This class is used for logging to the console. The macros defined below return an instance of
 * this class, which they can then print through using the << operator. The logger will
 * automatically insert a newline at the end of the output. If instantiated with exit=true, the
 * logger aborts the program when output is finished.
 */
class logger {
 public:
  /// When set, disable color terminal output
  inline static bool disable_color = false;

  /// When set, include source locations in log messages
  inline static bool debug = false;

  /// The threshold for log message output
  inline static LogLevel log_level = LogLevel::Fatal;

 private:
  LogLevel _level;  // Should the program abort when the log message is finished?
  bool _done;       // Is this the final command in the log output?

 public:
  logger(const char* source_file, int source_line, LogLevel level) noexcept :
      _level(level), _done(true) {
    // Only log things if they're at or above our log threshold
    if (_level <= log_level) {
      // Print source information, if enabled
      if (debug) {
        if (!disable_color) cerr << COLOR_SOURCE;
        cerr << "[" << source_file << ":" << source_line << "] ";
      }

      // Set the log color
      if (!disable_color) {
        if (_level == LogLevel::Verbose) {
          cerr << COLOR_VERBOSE;
        } else if (_level == LogLevel::Info) {
          cerr << COLOR_INFO;
        } else if (_level == LogLevel::Warning) {
          cerr << COLOR_WARNING;
        } else if (_level == LogLevel::Fatal) {
          cerr << COLOR_FATAL;
        }
      }
    }
  }

  logger(logger&& other) noexcept {
    _level = other._level;
    _done = other._done;
    other._done = false;
  }

  ~logger() noexcept {
    if (_done) {
      // If this log message is being displayed, end color output and print a newline
      if (static_cast<int>(_level) <= static_cast<int>(log_level)) {
        if (!disable_color) cerr << COLOR_END;
        cerr << "\n";
      }

      // If this log is a fatal
      if (_level == LogLevel::Fatal) {
        // In debug mode, call abort() so we can run a backtrace. Otherwise exit with failure.
        if (debug)
          abort();
        else
          exit(2);
      }
    }
  }

  void operator=(logger&& other) noexcept {
    _level = other._level;
    _done = other._done;
    other._done = false;
  }

  logger&& indent(size_t n, size_t tab_size = 2) noexcept {
    if (_level <= log_level) {
      for (size_t i = 0; i < n; i++) {
        for (size_t j = 0; j < tab_size; j++) {
          cerr << " ";
        }
      }
    }
    return move(*this);
  }

  template <typename T>
  logger&& operator<<(T t) noexcept {
    if (_level <= log_level) cerr << t;
    return move(*this);
  }
};

class null_logger {
 public:
  template <typename T>
  null_logger& operator<<(T t) noexcept {
    return *this;
  }
};

// Set macros for explicit logging
#define LOG logger(__FILE__, __LINE__, LogLevel::Verbose)
#define INFO logger(__FILE__, __LINE__, LogLevel::Info)
#define WARN logger(__FILE__, __LINE__, LogLevel::Warning)
#define FAIL logger(__FILE__, __LINE__, LogLevel::Fatal)

// Define conditional logging macros
#define INFO_IF(cond) \
  if (cond) INFO
#define INFO_UNLESS(cond) \
  if (!(cond)) INFO

#define WARN_IF(cond) \
  if (cond) WARN
#define WARN_UNLESS(cond) \
  if (!(cond)) WARN

#define FAIL_IF(cond) \
  if (cond) FAIL
#define FAIL_UNLESS(cond) \
  if (!(cond)) FAIL

// Define a shortcut for printing the error message corresponding to the current errno
#define ERR strerror(errno)

// NDEBUG-controlled ASSERT and PREFER macros
#ifdef NDEBUG
#define ASSERT(cond) null_logger()
#define PREFER(cond) null_logger()
#else
#define ASSERT(cond) FAIL_UNLESS(cond)
#define PREFER(cond) WARN_UNLESS(cond)
#endif
