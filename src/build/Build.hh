#pragma once

#include <memory>
#include <ostream>
#include <set>
#include <vector>

#include "build/BuildObserver.hh"
#include "build/Env.hh"
#include "data/Command.hh"

using std::ostream;
using std::set;
using std::shared_ptr;
using std::vector;

class Version;

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
  Build(shared_ptr<Command> root) : _root(root), _env(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /**
   * Mark a command for re-execution rather than emulation
   * \param c The command that should be executed rather than emulated
   * \returns true if this is a new addition to the set, or false if the command was already marked
   */
  bool setRerun(const shared_ptr<Command>& c) {
    auto [iter, added] = _rerun.insert(c);
    return added;
  }

  /**
   * Check if a command is marked for re-execution rather than emulation
   * \param c The command that is being checked
   * \returns true if the command is marked for reexecution, otherwise false
   */
  bool checkRerun(const shared_ptr<Command>& c) const { return _rerun.find(c) != _rerun.end(); }

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
  void launch(const shared_ptr<Command>& parent, const shared_ptr<Command>& child);

  /// Print information about this build
  ostream& print(ostream& o) const;

  friend ostream& operator<<(ostream& o, const Build& b) { return b.print(o); }

  friend ostream& operator<<(ostream& o, const Build* b) { return b->print(o); }

  /********** Observer Interface **********/

  /// Add an observer to this build
  void addObserver(shared_ptr<BuildObserver> o) { _observers.push_back(o); }

  /// Inform the observer that command c modified artifact a, creating version v
  void observeOutput(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                     const shared_ptr<Version>& v) {
    for (auto& o : _observers) o->output(c, a, v);
  }

  /// Inform the observer that command c accessed version v of artifact a
  void observeInput(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                    const shared_ptr<Version>& v) {
    for (auto& o : _observers) o->input(c, a, v);
  }

  /// Inform the observer that command c did not find the expected version in artifact a
  /// Instead of version `expected`, the command found version `observed`
  void observeMismatch(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                       const shared_ptr<Version>& observed, const shared_ptr<Version>& expected) {
    for (auto& o : _observers) o->mismatch(c, a, observed, expected);
  }

  /// Inform observers that a command has never been run
  void observeCommandNeverRun(const shared_ptr<Command>& c) {
    for (auto& o : _observers) o->commandNeverRun(c);
  }

  /// Inform the observer that a given command's IR action would detect a change in the build env
  void observeCommandChange(const shared_ptr<Command>& c, const shared_ptr<const Step>& s) {
    for (auto& o : _observers) o->commandChanged(c, s);
  }

  /// Inform observers that an artifact's version does not match the expected final state
  void observeFinalMismatch(const shared_ptr<Artifact>& a, const shared_ptr<Version>& observed,
                            const shared_ptr<Version>& expected) {
    for (auto& o : _observers) o->finalMismatch(a, observed, expected);
  }

 private:
  void runCommand(const shared_ptr<Command>& c);

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