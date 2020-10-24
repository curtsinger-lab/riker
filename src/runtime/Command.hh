#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include "data/AccessFlags.hh"
#include "versions/Version.hh"

using std::list;
using std::map;
using std::optional;
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
 private:
  friend class RebuildPlanner;

  /// Create a command with no values filled in. This is only used to create the null command
  Command() noexcept = default;

 public:
  /// The type of a command ID
  using ID = uint32_t;

  /// The type of a reference ID
  using RefID = uint32_t;

  /// Create a new command
  Command(shared_ptr<Ref> exe,
          shared_ptr<Ref> initial_cwd,
          shared_ptr<Ref> initial_root,
          map<int, shared_ptr<Ref>> initial_fds,
          vector<string> args) noexcept :
      _exe(exe),
      _initial_cwd(initial_cwd),
      _initial_root(initial_root),
      _initial_fds(initial_fds),
      _args(args) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) noexcept = default;
  Command& operator=(Command&&) noexcept = default;

  /// Get a shared pointer to the special null command instance
  static shared_ptr<Command> getNullCommand() noexcept;

  /// Get a short, printable name for this command
  string getShortName(size_t limit = 20) const noexcept;

  /// Get the full name for this command
  string getFullName() const noexcept;

  /// Is this command the make build tool?
  bool isMake() const noexcept;

  /// Get the reference to the executable file this command runs
  shared_ptr<Ref> getExecutable() const noexcept { return _exe; }

  /// Get the working directory where this command is started
  shared_ptr<Ref> getInitialWorkingDir() const noexcept { return _initial_cwd; }

  /// Get the root directory in effect when this command is started
  shared_ptr<Ref> getInitialRootDir() const noexcept { return _initial_root; }

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
  const map<int, shared_ptr<Ref>>& getInitialFDs() const noexcept { return _initial_fds; }

  /*
  /// When we emulate this command's launch of a child command, keep a record so we can match
  /// against it later and possibly skip that child command.
  void addChild(shared_ptr<Command> child) noexcept;

  /// Look through this command's list of children to see if there is a matching child
  shared_ptr<Command> findChild(shared_ptr<Ref> exe_ref,
                                vector<string> args,
                                map<int, shared_ptr<Ref>> fds,
                                shared_ptr<Ref> cwd_ref,
                                shared_ptr<Ref> root_ref) noexcept;
  */

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) noexcept {
    if (!c._exe) {
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
  /// The executable file this command runs
  shared_ptr<Ref> _exe;

  /// A reference to the directory where this command is started
  shared_ptr<Ref> _initial_cwd;

  /// A reference to the root directory in effect when this command is started
  shared_ptr<Ref> _initial_root;

  /// The file descriptor table at the start of this command's execution
  map<int, shared_ptr<Ref>> _initial_fds;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// Has this command ever run?
  bool _executed = false;

  /// The exit status recorded for this command after its last execution
  int _exit_status;

  /*
  /// A record of a past command launch
  struct ChildRecord {
    shared_ptr<Version> _exe_content;
    optional<fs::path> _cwd_path;
    vector<string> _args;
    map<int, shared_ptr<Version>> _fd_content;
    shared_ptr<Command> _command;

    ChildRecord(shared_ptr<Command> child) noexcept;

    ChildRecord(shared_ptr<Ref> exe_ref,
                shared_ptr<Ref> cwd_ref,
                vector<string> args,
                map<int, shared_ptr<Ref>> fds) noexcept;

    bool operator==(const ChildRecord& other) noexcept;
  };

  /// A set of commands launched by this command
  list<ChildRecord> _children;
  */
};
