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
#include "versions/ContentVersion.hh"

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
class MetadataVersion;
class Ref;

enum class InputType {
  PathResolution,  // The input is a dependency for path resolution
  Accessed,        // The input is accessed directly
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

  /****** Dependency Tracking ******/

  /// This command observes a change in a given scenario
  void observeChange(Scenario s) noexcept;

  /// An input to this command did not match the expected version
  void inputChanged(shared_ptr<Artifact> artifact,
                    shared_ptr<MetadataVersion> observed,
                    shared_ptr<MetadataVersion> expected,
                    Scenario scenario) noexcept;

  /// An input to this command did not match the expected version
  void inputChanged(shared_ptr<Artifact> artifact,
                    shared_ptr<ContentVersion> observed,
                    shared_ptr<ContentVersion> expected,
                    Scenario scenario) noexcept;

  /// Get the set of scenarios where this command has observed a change
  const set<Scenario>& getChanged() const noexcept { return _changed; }

  /// Track an input to this command
  void addMetadataInput(shared_ptr<Artifact> a, InputType t) noexcept;

  /// Track an input to this command
  void addContentInput(shared_ptr<Artifact> a, shared_ptr<ContentVersion> v, InputType t) noexcept;

  /// Get the inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<MetadataVersion>, InputType>> getMetadataInputs()
      const noexcept {
    return _metadata_inputs;
  }

  /// Get the inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<ContentVersion>, InputType>> getContentInputs()
      const noexcept {
    return _content_inputs;
  }

  /// Track an output from this command
  void addMetadataOutput(shared_ptr<Artifact> a, shared_ptr<MetadataVersion> v) noexcept;

  /// Track an output from this command
  void addContentOutput(shared_ptr<Artifact> a, shared_ptr<ContentVersion> v) noexcept;

  /// Get the outputs from this command
  const set<tuple<shared_ptr<Artifact>, shared_ptr<MetadataVersion>>>& getMetadataOutputs()
      const noexcept {
    return _metadata_outputs;
  }

  /// Get the outputs from this command
  const set<tuple<shared_ptr<Artifact>, shared_ptr<ContentVersion>>>& getContentOutputs()
      const noexcept {
    return _content_outputs;
  }

  /// Get the users of this command's outputs
  const set<shared_ptr<CommandRun>>& getOutputUsers() const noexcept { return _output_used_by; }

  /// An output from this command does not match the on-disk state (checked at the end of the build)
  void outputChanged(shared_ptr<Artifact> artifact,
                     shared_ptr<ContentVersion> ondisk,
                     shared_ptr<ContentVersion> expected) noexcept;

 private:
  /// The command this run is associated with
  shared_ptr<Command> _command;

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

  /// The set of inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<MetadataVersion>, InputType>> _metadata_inputs;

  /// The set of inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<ContentVersion>, InputType>> _content_inputs;

  /// The set of outputs from this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<MetadataVersion>>> _metadata_outputs;

  /// The set of outputs from this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<ContentVersion>>> _content_outputs;

  /// The set of command runs that use this command's outputs
  set<shared_ptr<CommandRun>> _output_used_by;
};