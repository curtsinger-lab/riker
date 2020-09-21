#pragma once

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <utility>

#include <fcntl.h>
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

// Printing unique_ptr values is allowed
template <class T>
static std::ostream& operator<<(std::ostream& o, const std::unique_ptr<T>& p) {
  return o << p.get();
}

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
  bool _enabled;  // Should this logger print anything?
  bool _abort;    // Should the program abort when the log message is finished?
  bool _done;     // Is this the final command in the log output?

 public:
  logger(const char* source_file,
         int source_line,
         LogCategory category,
         const char* category_name) noexcept :
      _enabled(static_cast<bool>(logger::log_categories & static_cast<int>(category))),
      _abort(category == LogCategory::error),
      _done(true) {
    // Stop immediately if this logger is not enabled
    if (!_enabled) return;

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
    if (_enabled && _done) {
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
    if (_enabled) cerr << t;
    return move(*this);
  }

  using StreamType = decltype(std::cerr);
  using EndlType = StreamType& (*)(StreamType&);

  logger&& operator<<(EndlType e) { return move(*this); }

  /// Decodes the given flag and appends it to the given std:string.
  static std::string fcntl_decode(int flags) {
    std::string s("");
    bool noflag = true;

    // decode O_RDONLY, O_WRONLY, O_RDWR
    if ((flags & O_WRONLY) == O_WRONLY) {
      s.append("O_WRONLY");
      noflag = false;
    } else if ((flags & O_RDWR) == O_RDWR) {
      s.append("O_RDWR");
      noflag = false;
    } else {
      s.append("O_RDONLY");
      noflag = false;
    }

    // pretty printer
    auto dec = [](std::string& s, int flags, int flag, const char* fstr, bool noflag) {
      if ((flags & flag) == flag) {
        if (!noflag) s.append("|");
        s.append(fstr);
        return false;
      }
      return noflag;
    };

    // decode the rest
    noflag = dec(s, flags, O_CREAT, "O_CREAT", noflag);
    noflag = dec(s, flags, O_EXCL, "O_EXCL", noflag);
    noflag = dec(s, flags, O_NOCTTY, "O_NOCTTY", noflag);
    noflag = dec(s, flags, O_TRUNC, "O_TRUNC", noflag);
    noflag = dec(s, flags, O_DIRECTORY, "O_DIRECTORY", noflag);
    noflag = dec(s, flags, O_NOFOLLOW, "O_NOFOLLOW", noflag);
    noflag = dec(s, flags, O_CLOEXEC, "O_CLOEXEC", noflag);
    noflag = dec(s, flags, O_TMPFILE, "O_TMPFILE", noflag);

    // append flags in octal
    s.append(fmt::format(" ({:o})", flags));

    return s;
  }
};

// Is a given log type enabled?
#define LOG_ENABLED(type) \
  static_cast<bool>(logger::log_categories & static_cast<int>(LogCategory::type))

#define LOG(type) logger(__FILE__, __LINE__, LogCategory::type, #type)

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
