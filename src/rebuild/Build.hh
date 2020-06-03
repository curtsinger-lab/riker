#pragma once

#include <memory>
#include <ostream>
#include <set>
#include <vector>

#include "data/Command.hh"
#include "rebuild/BuildObserver.hh"
#include "rebuild/Env.hh"

using std::ostream;
using std::set;
using std::shared_ptr;
using std::vector;

/**
 * A Build instance manages the execution of a build. This instance is responsible for setting up
 * the build environment, emulating or running each of the commands, and notifying any observers of
 * dependencies and changes detected during the build.
 */
class Build {
 public:
  /**
   * Create a build that runs a root command and its descendants
   * By default, this build will emulate all commands. Commands that must be re-executed can be
   * added later.
   * \param root  The root command of the build
   */
  Build(shared_ptr<Command> root) : _root(root), _env(this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  // Allow Move
  Build(Build&&) = default;
  Build& operator=(Build&&) = default;

  /**
   * Mark a command for re-execution rather than emulation
   * \param c The command that should be executed rather than emulated
   * \returns true if this is a new addition to the set, or false if the command was already marked
   */
  bool setRerun(shared_ptr<Command> c) {
    auto [iter, added] = _rerun.insert(c);
    return added;
  }

  /**
   * Check if a command is marked for re-execution rather than emulation
   * \param c The command that is being checked
   * \returns true if the command is marked for reexecution, otherwise false
   */
  bool checkRerun(shared_ptr<Command> c) const { return _rerun.find(c) != _rerun.end(); }

  /**
   * Get the set of commands that are marked for rerun in this build.
   * \returns a set of commands to rerun
   */
  const set<shared_ptr<Command>>& getRerun() const { return _rerun; }

  /**
   * Run this build as specified
   */
  void run();

  /**
   * Get the environment used for this build.
   * \returns a reference to the environment in the build
   */
  Env& getEnv() { return _env; }

  /**
   * Tell the build to launch a command
   * \param parent The parent of the launched command
   * \param child  The newly-launched command
   */
  void launch(shared_ptr<Command> parent, shared_ptr<Command> child);

  /// Print information about this build
  ostream& print(ostream& o) const;

  friend ostream& operator<<(ostream& o, const Build& b) { return b.print(o); }

  friend ostream& operator<<(ostream& o, const Build* b) { return b->print(o); }

  /********** Observer Interface **********/

  /// Add an observer to this build
  void addObserver(shared_ptr<BuildObserver> o) { _observers.push_back(o); }

  /// Inform the observer that command c modified the metadata of artifact a
  void observeMetadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->metadataOutput(c, a);
  }

  /// Inform the observer that command c modified the contents of artifact a
  void observeContentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->contentOutput(c, a);
  }

  /// Inform the observer that command c accessed the metadata of artifact a
  void observeMetadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->metadataInput(c, a);
  }

  /// Inform the observer that command c accessed the contents of artifact a
  void observeContentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->contentInput(c, a);
  }

  /// Inform the observer that command c did not find the expected metadata in artifact a
  void observeMetadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->metadataMismatch(c, a);
  }

  /// Inform the observer that command c did not find the expected contents in artifact a
  void observeContentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->contentMismatch(c, a);
  }

  /// Inform observers that a command has never been run
  void observeCommandNeverRun(shared_ptr<Command> c) {
    for (auto& o : _observers) o->commandNeverRun(c);
  }

  /// Inform the observer that a given command's IR action would detect a change in the build env
  void observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) {
    for (auto& o : _observers) o->commandChanged(c, s);
  }

  /// Inform the observer that an artifact's metadata does not match the expected final state
  void observeFinalMetadataMismatch(shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->finalMetadataMismatch(a);
  }

  /// Inform the observer that an artifact's contents do not match the expected final state
  void observeFinalContentMismatch(shared_ptr<Artifact> a) {
    for (auto& o : _observers) o->finalContentMismatch(a);
  }

 private:
  void runCommand(shared_ptr<Command> c);

 private:
  /// The root command of this build
  shared_ptr<Command> _root;

  /// The environment in which this build executes
  Env _env;

  /// Commands that should be executed rather than emulated
  set<shared_ptr<Command>> _rerun;

  /// The observers that should be notified of dependency and change information during the build
  vector<shared_ptr<BuildObserver>> _observers;
};