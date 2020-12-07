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
class Ref;
class Step;

/// Record the reason why a command has been marked for rerun. Reasons are ordered; any command
/// marked with both Child and Changed will retain the Changed marking.
enum class RerunReason : int {
  Child = 0,           // The marked command is a child of another command marked for rerun
  InputMayChange = 1,  // The marked command consumes output from another command marked for rerun
  OutputNeeded = 2,    // The marked command produces output needed by another marked command
  Changed = 3          // The marked command directly observed a change
};

enum class InputType {
  PathResolution,  // The input is a dependency for path resolution
  Accessed,        // The input is accessed directly
  Exists,          // The input must exist, but its specific contents do not matter
};

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
  Command() noexcept;

 public:
  /// The type of a command ID
  using ID = uint32_t;

  /// The type of a reference ID
  using RefID = uint32_t;

  /// Default RefIDs
  enum : RefID { StdinRef = 0, StdoutRef = 1, StderrRef = 2, RootRef = 3, CwdRef = 4, ExeRef = 5 };

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
  const map<int, RefID>& getInitialFDs() const noexcept { return _initial_fds; }

  /// Add an initial file descriptor to this command
  void addInitialFD(int fd, Command::RefID ref) noexcept {
    ASSERT(fd >= 0) << "Invalid file descriptor number " << fd << " in " << this;
    _initial_fds.emplace(fd, ref);
  }

  /// Finish the current run of this command. This moves the run data to last_run.
  void finishRun() noexcept;

  /// Get a reference from this command's reference table
  const shared_ptr<Ref>& getRef(RefID id) const noexcept;

  /// Store a reference at a known index of this command's local reference table
  void setRef(RefID id, shared_ptr<Ref> ref) noexcept;

  /// Store a reference at the next available index of this command's local reference table
  RefID setRef(shared_ptr<Ref> ref) noexcept;

  /// Increment the use count for a Ref. Return true if this is the first use of the ref.
  bool usingRef(RefID id) noexcept;

  /// Decrement a use count for a Ref. Return true if this was the last use of the ref.
  bool doneWithRef(RefID id) noexcept;

  /// Get this command's exit status
  int getExitStatus() const noexcept;

  /// Set this command's exit status, and record that it has exited
  void setExitStatus(int status) noexcept;

  /// Create dependencies to prepare this command for execution
  void createLaunchDependencies(Build& build) noexcept;

  /**
   * Remember that this command launched a child command. This is used on the next run to match
   * launched children against commands fromn the previous run.
   */
  void addChild(shared_ptr<Command> child) noexcept;

  /// Get this command's list of children
  const list<shared_ptr<Command>>& getChildren() const noexcept;

  /**
   * Look through this command's children from the last run to see if there is a child that matches
   * the given command launch information. Once a child has been matched, it will not match again.
   *
   * \param args      The arguments to the child command
   * \param exe_ref   This command's reference to the child command's executable
   * \param cwd_ref   This command's reference to the child command's working directory
   * \param root_ref  This command's reference to the child command's root directory
   * \param fds       The child command's initial file descriptors, and the reference (in this
   *                  command) they are initialized with.
   * \returns A pointer to the matched command, or nullptr if no child matches.
   */
  shared_ptr<Command> findChild(vector<string> args,
                                Command::RefID exe_ref,
                                Command::RefID cwd_ref,
                                Command::RefID root_ref,
                                map<int, Command::RefID> fds) noexcept;

  /****** Dependency Tracking and Rebuild Planning ******/

  /// Mark this command for rerun. Returns true if this is a new marking.
  bool markForRerun(RerunReason reason) noexcept;

  /// Check to see if this command was marked for re-execution after its previous run
  bool mustRerun() const noexcept;

  /// Track an input to this command
  void addInput(shared_ptr<Artifact> a, shared_ptr<Version> v, InputType t) noexcept;

  /// Get the inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>, InputType>> getInputs() const noexcept;

  /// Track an output from this command
  void addOutput(shared_ptr<Artifact> a, shared_ptr<Version> v) noexcept;

  /// Get the outputs from this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>>> getOutputs() const noexcept;

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
  map<int, RefID> _initial_fds;

  /// Has this command ever run?
  bool _executed = false;

  // Forward-declare the RunData struct, implemented in Command.cc
  struct RunData;

  /// Transient data for the current run
  unique_ptr<RunData> _run;

  /// Transient data for the last run
  unique_ptr<RunData> _last_run;
};
