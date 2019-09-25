#pragma once

#include <cerrno>
#include <cstdlib>
#include <iostream>
#include <utility>

#include "ui/options.hh"

#define COLOR_VERBOSE "\033[00;32m"
#define COLOR_INFO "\033[01;34m"
#define COLOR_WARNING "\033[01;33m"
#define COLOR_FATAL "\033[01;31m"
#define COLOR_SOURCE "\033[34m"
#define COLOR_END "\033[0m"

/**
 * This class is used for logging to the console. The macros defined below return an instance of
 * this class, which they can then print through using the << operator. The logger will
 * automatically insert a newline at the end of the output. If instantiated with exit=true, the
 * logger aborts the program when output is finished.
 */
class logger {
 private:
  log_level _level;  // Should the program abort when the log message is finished?
  bool _done;        // Is this the final command in the log output?

 public:
  logger(const char* source_file, int source_line, log_level level) : _level(level), _done(true) {
    // Only log things if they're at or above our log threshold
    if (_level >= options.log_threshold) {
      // Print source information, if enabled
      if (options.log_source_locations) {
        if (options.color_output) std::cerr << COLOR_SOURCE;
        std::cerr << "[" << source_file << ":" << source_line << "] ";
      }

      // Set the log color
      if (options.color_output) {
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
  }

  logger(logger&& other) {
    _level = other._level;
    _done = other._done;
    other._done = false;
  }

  ~logger() {
    if (_done) {
      // If this log message is being displayed, end color output and print a newline
      if (_level >= options.log_threshold) {
        if (options.color_output) std::cerr << COLOR_END;
        std::cerr << "\n";
      }

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
#define INFO_IF(cond) if (cond) INFO
#define INFO_UNLESS(cond) if (!(cond)) INFO

#define WARN_IF(cond) if (cond) WARN
#define WARN_UNLESS(cond) if (!(cond)) WARN

#define FAIL_IF(cond) if (cond) FAIL
#define FAIL_UNLESS(cond) if (!(cond)) FAIL

// Define a shortcut for printing the error message corresponding to the current errno
#define ERR strerror(errno)
