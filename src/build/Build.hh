#pragma once

#include <memory>
#include <ostream>
#include <set>
#include <vector>

#include "build/BuildObserver.hh"
#include "build/Env.hh"
#include "core/Command.hh"
#include "tracing/Tracer.hh"

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
  Build(shared_ptr<Command> root) noexcept : _root(root), _env(*this), _tracer(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /**
   * Mark a command for re-execution rather than emulation
   * \param c The command that should be executed rather than emulated
   * \returns true if this is a new addition to the set, or false if the command was already marked
   */
  bool setRerun(shared_ptr<Command> c) noexcept {
    auto [iter, added] = _rerun.emplace(c);
    return added;
  }

  /**
   * Check if a command is marked for re-execution rather than emulation
   * \param c The command that is being checked
   * \returns true if the command is marked for reexecution, otherwise false
   */
  bool checkRerun(shared_ptr<Command> c) const noexcept { return _rerun.find(c) != _rerun.end(); }

  /**
   * Get the set of commands that are marked for rerun in this build.
   * \returns a set of commands to rerun
   */
  const set<shared_ptr<Command>>& getRerun() const noexcept { return _rerun; }

  /**
   * Run this build as specified
   */
  void run() noexcept;

  /**
   * Get the environment used for this build.
   * \returns a reference to the environment in the build
   */
  Env& getEnv() noexcept { return _env; }

  /**
   * Tell the build to launch a command
   * \param parent The parent of the launched command
   * \param child  The newly-launched command
   */
  void launch(shared_ptr<Command> parent, shared_ptr<Command> child) noexcept;

  /**
   * Tell the build to join with a command
   * \param parent The command that is waiting
   * \param child  The child being joined
   */
  void join(shared_ptr<Command> child) noexcept;

  /// Print information about this build
  ostream& print(ostream& o) const noexcept;

  friend ostream& operator<<(ostream& o, const Build& b) noexcept { return b.print(o); }

  friend ostream& operator<<(ostream& o, const Build* b) noexcept { return b->print(o); }

  /********** Observer Interface **********/

  /// Add an observer to this build
  void addObserver(shared_ptr<BuildObserver> o) noexcept { _observers.push_back(o); }

  /// Inform the observer that command c modified artifact a, creating version v
  void observeOutput(shared_ptr<Command> c, shared_ptr<Artifact> a,
                     shared_ptr<Version> v) noexcept {
    for (const auto& o : _observers) o->output(c, a, v);
  }

  /// Inform the observer that command c accessed version v of artifact a
  void observeInput(shared_ptr<Command> c, shared_ptr<Artifact> a, shared_ptr<Version> v) noexcept {
    for (const auto& o : _observers) o->input(c, a, v);
  }

  /// Inform the observer that command c did not find the expected version in artifact a
  /// Instead of version `expected`, the command found version `observed`
  void observeMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a, shared_ptr<Version> observed,
                       shared_ptr<Version> expected) noexcept {
    for (const auto& o : _observers) o->mismatch(c, a, observed, expected);
  }

  /// Inform observers that a command has never been run
  void observeCommandNeverRun(shared_ptr<Command> c) noexcept {
    for (const auto& o : _observers) o->commandNeverRun(c);
  }

  /// Inform the observer that a given command's IR action would detect a change in the build env
  void observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) noexcept {
    for (const auto& o : _observers) o->commandChanged(c, s);
  }

  /// Inform observers that an artifact's version does not match the expected final state
  void observeFinalMismatch(shared_ptr<Artifact> a, shared_ptr<Version> observed,
                            shared_ptr<Version> expected) noexcept {
    for (const auto& o : _observers) o->finalMismatch(a, observed, expected);
  }

 private:
  void runCommand(shared_ptr<Command> c) noexcept;

 private:
  /// The root command of this build
  shared_ptr<Command> _root;

  /// The environment in which this build executes
  Env _env;

  /// The tracer that will be used to execute any commands that must rerun
  Tracer _tracer;

  /// A map of running commands to their root processes
  map<shared_ptr<Command>, shared_ptr<Process>> _running;

  /// Commands that should be executed rather than emulated
  set<shared_ptr<Command>> _rerun;

  /// The observers that should be notified of dependency and change information during the build
  vector<shared_ptr<BuildObserver>> _observers;
};