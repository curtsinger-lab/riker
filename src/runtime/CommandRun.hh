#pragma once

#include <cstdlib>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include "runtime/Ref.hh"

using std::list;
using std::map;
using std::optional;
using std::set;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;
using std::weak_ptr;

class Artifact;
class Build;
class Command;
class Ref;
class Version;

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
 * Predicates are tagged with specific scenarios where they apply.
 *
 * If all of a command's predicates in the Build scenario evaluate to true, the command does not
 * directly observe any change. The same is true for the PostBuild scenario.
 */
enum class Scenario { Build, PostBuild };

/**
 * Track data for a specific run of a command. This run could be emulated or traced.
 */
class CommandRun : public std::enable_shared_from_this<CommandRun> {
 public:
  /// Create a CommandRun with a back pointer to its owning command
  CommandRun(weak_ptr<Command> command) noexcept : _command(command) {}

  /// Get the command that produced this Run
  shared_ptr<Command> getCommand() const noexcept;

  /// Get a reference from this command's reference table
  const shared_ptr<Ref>& getRef(Ref::ID id) const noexcept;

  /// Store a reference at a known index of this command's local reference table
  void setRef(Ref::ID id, shared_ptr<Ref> ref) noexcept;

  /// Store a reference at the next available index of this command's local reference table
  Ref::ID setRef(shared_ptr<Ref> ref) noexcept;

  /// Increment the use count for a Ref. Return true if this is the first use of the ref.
  bool usingRef(Ref::ID id) noexcept;

  /// Decrement a use count for a Ref. Return true if this was the last use of the ref.
  bool doneWithRef(Ref::ID id) noexcept;

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
  void addChild(shared_ptr<CommandRun> child) noexcept;

  /// Get this command's list of children
  const list<shared_ptr<CommandRun>>& getChildren() const noexcept;

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
                                Ref::ID exe_ref,
                                Ref::ID cwd_ref,
                                Ref::ID root_ref,
                                map<int, Ref::ID> fds) noexcept;

  /****** Dependency Tracking and Rebuild Planning ******/

  /// Mark this command for rerun. Returns true if this is a new marking.
  bool markForRerun(RerunReason reason) noexcept;

  /// Check to see if this command was marked for re-execution after its previous run
  bool mustRerun() const noexcept;

  /// This command observes a change in a given scenario
  void observeChange(Scenario s) noexcept;

  /// An input to this command did not match the expected version
  void inputChanged(shared_ptr<Artifact> artifact,
                    shared_ptr<Version> observed,
                    shared_ptr<Version> expected,
                    Scenario scenario) noexcept;

  /// Get the set of scenarios where this command has observed a change
  const set<Scenario>& getChanged() const noexcept { return _changed; }

  /// Track an input to this command
  void addInput(shared_ptr<Artifact> a, shared_ptr<Version> v, InputType t) noexcept;

  /// Get the inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>, InputType>> getInputs() const noexcept {
    return _inputs;
  }

  /// Track an output from this command
  void addOutput(shared_ptr<Artifact> a, shared_ptr<Version> v) noexcept;

  /// Get the outputs from this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>>> getOutputs() const noexcept {
    return _outputs;
  }

  /// Get the users of this command's outputs
  const set<shared_ptr<CommandRun>>& getOutputUsers() const noexcept { return _output_used_by; }

  /// An output from this command does not match the on-disk state (checked at the end of the build)
  void outputChanged(shared_ptr<Artifact> artifact,
                     shared_ptr<Version> ondisk,
                     shared_ptr<Version> expected) noexcept;

 private:
  /// The command this run is associated with
  weak_ptr<Command> _command;

  /// The command's local references
  vector<shared_ptr<Ref>> _refs;

  /// This command's use countn for each of its references
  vector<size_t> _refs_use_count;

  /// The children launched by this command
  list<shared_ptr<CommandRun>> _children;

  /// Has this command run already been matched against a new command launch?
  bool _matched = false;

  /// The exit status for this command
  int _exit_status = -1;

  /// Keep track of the scenarios where this command has observed a change
  set<Scenario> _changed;

  /// If this command is marked for re-execution, the optional will have a value
  optional<RerunReason> _rerun_reason;

  /// The set of inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>, InputType>> _inputs;

  /// The set of outputs from this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>>> _outputs;

  /// The set of command runs that use this command's outputs
  set<shared_ptr<CommandRun>> _output_used_by;
};