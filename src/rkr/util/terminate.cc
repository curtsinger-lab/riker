#include "terminate.hh"

#include <list>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include <errno.h>
#include <execinfo.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <unistd.h>

#include "util/log.hh"
#include "util/wrappers.hh"

using std::list;
using std::string;
using std::stringstream;
using std::tuple;
using std::vector;

static pid_t main_process = getpid();

static void signal_handler(int sig, siginfo_t* info, void* ctx);

__attribute__((constructor)) void init() {
  struct sigaction sa;
  memset(&sa, 0, sizeof(struct sigaction));
  sa.sa_sigaction = signal_handler;

  // Set signal handlers
  for (int sig : {SIGTERM, SIGINT, SIGQUIT, SIGTSTP}) {
    if (sigaction(sig, &sa, nullptr)) {
      perror("Failed to set handler");
      exit(2);
    }
  }
}

static string wrap(string line, int cols, const char* indent = "                    ") {
  if (line.size() < cols) {
    return line;
  }

  // Hard cut the maximum length that fits on one line
  auto front = line.substr(0, cols);

  // Look for a nice delimeter in the first line
  auto delimeter_pos = front.find_last_of(", >:");

  // Is there a delimeter within the last quarter of the line?
  if (delimeter_pos != string::npos && 3 * cols <= 4 * delimeter_pos) {
    // Break at the delimeter
    auto first = line.substr(0, delimeter_pos);
    auto rest = line.substr(delimeter_pos);

    // If the delimieter was a space, strip it off the front of the remaining string
    if (rest[0] == ' ') rest = rest.substr(1);

    // Continue wrapping and return
    return first + "\n" + wrap(indent + rest, cols);

  } else {
    // Hard wrap at the terminal width
    return line.substr(0, cols) + "\n" + wrap(indent + line.substr(cols), cols);
  }
}

static string filter(string line) {
  static list<tuple<string, string>> substitutions = {
      {"> >", ">>"},
      {"__cxx11::", ""},
      {"__gnu_cxx::", ""},
      {"std::", ""},
      {"basic_string<char, char_traits<char>, allocator<char>>", "string"},
      {"filesystem::path", "fs::path"},
      {", allocator<string>", ""},
      {", allocator<CLI::App*>", ""},
      {"operator!=<CLI::App**, vector<CLI::App*>>", "operator!="},
      {"__normal_iterator", "iterator"},
      {"at ??:?", "at <unknown source location>"},
      {"?? ??:0", "<unknown symbol>"},
      {":?\n", ":<unknown line>\n"},
      {"(inlined by)", "        (inlined):"}};

  for (const auto& [pattern, replacement] : substitutions) {
    size_t pos;
    while ((pos = line.find(pattern)) != string::npos) {
      line.replace(pos, pattern.size(), replacement);
    }
  }

  // Try to get the terminal width
  struct winsize size;
  if (ioctl(STDERR_FILENO, TIOCGWINSZ, &size) == 0) {
    return wrap(line, size.ws_col);
  } else {
    return wrap(line, 80);
  }
}

#define BACKTRACE_SIZE 256

static void print_backtrace(int skip) {
  void* buf[BACKTRACE_SIZE];
  int count = backtrace(buf, BACKTRACE_SIZE);

  vector<string> backtrace_lines;

  if (skip < count) {
    auto main_exe = readlink("/proc/self/exe");

    // Use a stringstream to build an addr2line invocation
    stringstream ss;
    ss << "addr2line -aspifC -e " << main_exe;

    // Add each address to the command line
    for (int i = skip; i < count; i++) {
      ss << " " << buf[i];
    }

    // Create a pipe for addr2line to write to
    int pipe_fds[2];
    if (pipe(pipe_fds)) {
      perror("pipe failed");
      return;
    }

    // Fork a child to run addr2line
    auto child = fork();

    // Are we in the child?
    if (child == 0) {
      // Close the read end of the pipe
      close(pipe_fds[0]);

      // Map the pipe's write end to stdout
      if (dup2(pipe_fds[1], 1) != 1) {
        perror("dup2 failed");
        exit(2);
      }

      // Open /dev/null to mute stderr
      int null_fd = open("/dev/null", O_WRONLY);

      // Remap the /dev/null fd to stderr
      if (dup2(null_fd, 2) != 2) {
        perror("dup2 failed");
        exit(2);
      }

      int rc = system(ss.str().c_str());

      if (rc != 0) {
        backtrace_symbols_fd(buf, count, STDOUT_FILENO);
        printf("Warning: unable to launch addr2line. Using backtrace_symbols as a fallback.\n");
      }

      exit(0);

    } else {
      // Close the write end of the pipe
      close(pipe_fds[1]);

      // Create an fstream to read from the pipe
      FILE* stream = fdopen(pipe_fds[0], "r");
      if (stream == nullptr) {
        perror("fdopen failed");
        return;
      }

      // Set up for getline
      char* line = nullptr;
      size_t len = 0;
      ssize_t nread;

      // Read lines until the pipe is closed
      while ((nread = getline(&line, &len, stream)) != -1) {
        if (line[nread - 1] == '\n') line[nread - 1] = '\0';
        backtrace_lines.push_back(line);
      }
      free(line);
      fclose(stream);
    }
  }

  if (backtrace_lines.size() > 0) {
    fprintf(stderr, "Backtrace: \n");
    for (auto line : backtrace_lines) {
      // Do some quick cleanup on the line and print it
      fprintf(stderr, "%s\n", filter(line).c_str());
    }
  } else {
    fprintf(stderr, "Backtrace unavailable\n");
  }
}

void terminate() noexcept {
  // Is this the main program or a child process failing?
  pid_t current_process = getpid();
  if (current_process != main_process) {
    fprintf(stderr, "Failure occurred in child process %d\n", current_process);
    fprintf(stderr, "Sending termination signal to %d\n", main_process);

    // Send SIGTERM to the parent process
    kill(main_process, SIGTERM);
  }

  // Print an error backtrace, skipping the first two entries (this function and the backtrace
  // function)
  print_backtrace(2);

  // In debug mode, dump core and call abort(). Otherwise just exit.
  if (options::debug) {
    // Make sure we can save a core file
    struct rlimit limits = {.rlim_cur = RLIM_INFINITY, .rlim_max = RLIM_INFINITY};
    int rc = setrlimit(RLIMIT_CORE, &limits);

    if (rc == -1) {
      logger<LogCategory::warning>() << "Failed to enable a core dump. Run `ulimit -c "
                                        "unlimited` to save a core file on failure.";
    }

    abort();
  } else {
    exit(2);
  }
}

static void signal_handler(int sig, siginfo_t* info, void* ctx) {
  fprintf(stderr, "Caught termination signal in %d\n", getpid());
  terminate();
}
