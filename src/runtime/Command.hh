#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include "data/AccessFlags.hh"
#include "runtime/Ref.hh"
#include "versions/Version.hh"

using std::list;
using std::map;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::unique_ptr;
using std::vector;

namespace fs = std::filesystem;

class Artifact;
class Build;
class BuildObserver;
class CommandRun;
class Ref;
class Step;

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
 private:
  friend class RebuildPlanner;

  /// Default constructor used to create the null command instance
  Command() noexcept = default;

 public:
  /// The type of a command ID
  using ID = uint32_t;

  /// Create a new command
  Command(vector<string> args) noexcept;

  /// Declare a destructor
  ~Command() noexcept;

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) noexcept = default;
  Command& operator=(Command&&) noexcept = default;

  /// Get a shared pointer to the special null command instance
  static const shared_ptr<Command>& getNullCommand() noexcept;

  /// Get a short, printable name for this command
  string getShortName(size_t limit = 20) const noexcept;

  /// Get the full name for this command
  string getFullName() const noexcept;

  /// Is this command the null command?
  bool isNullCommand() const noexcept;

  /// Is this command the make build tool?
  bool isMake() const noexcept;

  /// Check if this command has ever executed
  bool hasExecuted() const noexcept { return _executed; }

  /// Record that this command has now been executed
  void setExecuted() noexcept { _executed = true; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const noexcept { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, Ref::ID>& getInitialFDs() const noexcept { return _initial_fds; }

  /// Add an initial file descriptor to this command
  void addInitialFD(int fd, Ref::ID ref) noexcept {
    ASSERT(fd >= 0) << "Invalid file descriptor number " << fd << " in " << this;
    _initial_fds.emplace(fd, ref);
  }

  /// Get the transient data for this command's current run
  const shared_ptr<CommandRun>& currentRun() noexcept;

  /// Get the transient data for this command's previous run
  const shared_ptr<CommandRun>& previousRun() noexcept;

  /// Finish the current run of this command. This moves the run data to last_run.
  void finishRun() noexcept;

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) noexcept {
    if (c.isNullCommand()) {
      return o << "[No Command]";
    } else {
      return o << "[Command " << c.getShortName() << "]";
    }
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) noexcept {
    if (c == nullptr) return o << "<null Command>";
    return o << *c;
  }

 private:
  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptor entries populated at the start of this command's execution
  map<int, Ref::ID> _initial_fds;

  /// Has this command ever run?
  bool _executed = false;

  /// Transient data for the current run
  shared_ptr<CommandRun> _run;

  /// Transient data for the last run
  shared_ptr<CommandRun> _last_run;
};
