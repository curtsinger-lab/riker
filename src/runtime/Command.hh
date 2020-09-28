#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "data/AccessFlags.hh"
#include "data/FileDescriptor.hh"

using std::list;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

class Artifact;
class Build;
class BuildObserver;
class Ref;
class Step;

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
  friend class RebuildPlanner;

 public:
  /// The type of a command ID
  using ID = uint32_t;

  /// Create a new command
  Command(shared_ptr<RefResult> exe,
          vector<string> args,
          map<int, FileDescriptor> initial_fds,
          shared_ptr<RefResult> initial_cwd,
          shared_ptr<RefResult> initial_root) noexcept :
      _exe(exe),
      _args(args),
      _initial_fds(initial_fds),
      _initial_cwd(initial_cwd),
      _initial_root(initial_root) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) noexcept = default;
  Command& operator=(Command&&) noexcept = default;

  /// Get a short, printable name for this command
  string getShortName(size_t limit = 20) const noexcept;

  /// Get the full name for this command
  string getFullName() const noexcept;

  /// Get the reference to the executable file this command runs
  shared_ptr<RefResult> getExecutable() const noexcept { return _exe; }

  /// Get the working directory where this command is started
  shared_ptr<RefResult> getInitialWorkingDir() const noexcept { return _initial_cwd; }

  /// Get the root directory in effect when this command is started
  shared_ptr<RefResult> getInitialRootDir() const noexcept { return _initial_root; }

  /// Check if this command has ever executed
  bool hasExecuted() const noexcept { return _executed; }

  /// Record that this command has now been executed
  void setExecuted() noexcept { _executed = true; }

  /// Get this command's exit status
  int getExitStatus() const noexcept { return _exit_status; }

  /// Set this command's exit status, and record that it has exited
  void setExitStatus(int status) noexcept { _exit_status = status; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const noexcept { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, FileDescriptor>& getInitialFDs() const noexcept { return _initial_fds; }

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) noexcept {
    return o << "[Command " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) noexcept {
    if (c == nullptr) return o << "<null Command>";
    return o << *c;
  }

 private:
  /// The executable file this command runs
  shared_ptr<RefResult> _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptor table at the start of this command's execution
  map<int, FileDescriptor> _initial_fds;

  /// A reference to the directory where this command is started
  shared_ptr<RefResult> _initial_cwd;

  /// A reference to the root directory in effect when this command is started
  shared_ptr<RefResult> _initial_root;

  /// Has this command ever run?
  bool _executed = false;

  /// The exit status recorded for this command after its last execution
  int _exit_status;
};