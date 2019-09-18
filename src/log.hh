#pragma once

#include <cerrno>
#include <cstring>
#include <iostream>

#include "options.hh"

#define COLOR_VERBOSE "\033[00;32m"
#define COLOR_INFO "\033[01;34m"
#define COLOR_WARNING "\033[01;33m"
#define COLOR_FATAL "\033[01;31m"
#define COLOR_SOURCE "\033[34m"
#define COLOR_END "\033[0m"

/**
 * This class provides the same interface as the logger class, but does not display anything.
 * Conditional logging (PREFER, REQUIRE, ASSERT) macros return an instance of this class when checks
 * pass so no message is displayed.
 */
class null_logger {
 public:
  null_logger indent(size_t n, size_t tab_size = 2) { return null_logger(); }

  template <typename T>
  null_logger operator<<(T t) {
    return null_logger();
  }
};

/**
 * This class is used for logging to the console. The macros defined below return an instance of
 * this class, which they can then print through using the << operator. The logger will
 * automatically insert a newline at the end of the output. If instantiated with exit=true, the
 * logger aborts the program when output is finished.
 *
 * This class must extend null_logger so conditional logging (PREFER and REQUIRE) can return a real
 * logger when a check fails, or a null logger when the check passes.
 */
class logger : public null_logger {
 private:
  log_level _level;  // Should the program abort when the log message is finished?
  bool _done;        // Is this the final command in the log output?

 public:
  logger(const char* source_file, int source_line, log_level level) : _level(level), _done(true) {
    // Only log things if they're at or above our log threshold
    if (_level >= options.log_threshold) {
      // Print source information, if enabled
      if (options.log_source_locations) {
        std::cerr << COLOR_SOURCE << "[" << source_file << ":" << source_line << "] ";
      }

      // Set the log color
      if (_level == log_level::Verbose) {
        std::cerr << COLOR_VERBOSE;
      } else if (_level == log_level::Info) {
        std::cerr << COLOR_INFO;
      } else if (_level == log_level::Warning) {
        std::cerr << COLOR_WARNING;
      } else if (_level == log_level::Fatal) {
        std::cerr << COLOR_FATAL;
      }
    }
  }

  logger(logger&& other) {
    _level = other._level;
    _done = other._done;
    other._done = false;
  }

  ~logger() {
    if (_done) {
      // If this log message is being displayed, end color output and print a newline
      if (_level >= options.log_threshold) std::cerr << COLOR_END << "\n";

      // If this log is a fatal
      if (_level == log_level::Fatal) exit(2);
    }
  }

  void operator=(logger&& other) {
    _level = other._level;
    _done = other._done;
    other._done = false;
  }

  logger&& indent(size_t n, size_t tab_size = 2) {
    if (_level >= options.log_threshold) {
      for (size_t i = 0; i < n; i++) {
        for (size_t j = 0; j < tab_size; j++) {
          std::cerr << " ";
        }
      }
    }
    return std::move(*this);
  }

  template <typename T>
  logger&& operator<<(T t) {
    if (_level >= options.log_threshold) std::cerr << t;
    return std::move(*this);
  }
};

// Set macros for explicit logging
#define LOG logger(__FILE__, __LINE__, log_level::Verbose)
#define INFO logger(__FILE__, __LINE__, log_level::Info)
#define WARN logger(__FILE__, __LINE__, log_level::Warning)
#define FAIL logger(__FILE__, __LINE__, log_level::Fatal)

// Define an ASSERT macro, but disable checks when NDEBUG is defined
#ifdef NDEBUG
#define ASSERT(cond) null_logger()
#else
#define ASSERT(cond) (cond) ? null_logger() : FAIL
#endif

// Define conditional logging macros
#define INFO_IF(cond) (cond) ? INFO : null_logger()
#define INFO_UNLESS(cond) (cond) ? null_logger() : INFO

#define WARN_IF(cond) (cond) ? WARN : null_logger()
#define WARN_UNLESS(cond) (cond) ? null_logger() : WARN

#define FAIL_IF(cond) (cond) ? FAIL : null_logger()
#define FAIL_UNLESS(cond) (cond) ? null_logger() : FAIL

// Define a shortcut for printing the error message corresponding to the current errno
#define ERR strerror(errno)
