#pragma once

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <utility>

#include <fmt/core.h>
#include <fmt/ostream.h>

using std::cerr;
using std::move;

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

  /// A bit field for the types of log messages that are enabled
  inline static int log_categories =
      static_cast<int>(LogCategory::error) | static_cast<int>(LogCategory::warning);

 private:
  bool _abort;  // Should the program abort when the log message is finished?
  bool _done;   // Is this the final command in the log output?

 public:
  logger(const char* source_file,
         int source_line,
         LogCategory category,
         const char* category_name) noexcept :
      _abort(category == LogCategory::error), _done(true) {
    // Set the color for the log category text
    if (!disable_color) {
      if (category == LogCategory::error) {
        cerr << FAINT RED;
      } else if (category == LogCategory::warning) {
        cerr << FAINT YELLOW;
      } else {
        cerr << FAINT GREEN;
      }
    }

    // Print the logging category name
    cerr << "(" << category_name << ") ";

    // Print source information, if enabled
    if (debug) {
      if (!disable_color) cerr << NORMAL BLUE;
      cerr << "[" << source_file << ":" << source_line << "] ";
    }

    // Set the log color for the actual message
    if (!disable_color) {
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
    if (_done) {
      // End color output and print a newline
      if (!disable_color) cerr << END_COLOR;
      cerr << "\n";
      cerr << std::dec;

      // If this log is a fatal
      if (_abort) {
        // In debug mode, call abort() so we can run a backtrace. Otherwise exit with failure.
        if (debug)
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
    cerr << t;
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

// Define the main logging macro
#define LOG(type)                                                   \
  if (logger::log_categories & static_cast<int>(LogCategory::type)) \
  logger(__FILE__, __LINE__, LogCategory::type, #type)

#define LOGF(type, format_str, ...) LOG(type) << fmt::format(format_str, __VA_ARGS__)

// Define shorthand macros for specific log types
#define WARN LOG(warning)
#define FAIL LOG(error)

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
