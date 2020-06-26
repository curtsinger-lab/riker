#pragma once

#include <memory>
#include <ostream>
#include <set>
#include <vector>

#include "build/AccessFilter.hh"
#include "build/BuildObserver.hh"
#include "build/Env.hh"
#include "build/Resolution.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "core/Trace.hh"
#include "tracing/Tracer.hh"

using std::ostream;
using std::set;
using std::shared_ptr;
using std::vector;

class Version;

enum class FingerprintLevel { None, Local, All };

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
  Build(shared_ptr<Trace> trace,
        FingerprintLevel fingerprint,
        bool print_on_run = false,
        bool dry_run = false) noexcept :
      _trace(trace),
      //_fingerprint(fingerprint),
      _print_on_run(print_on_run),
      _dry_run(dry_run),
      _env(*this),
      _tracer(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /// Mark command c for re-execution rather than emulation
  bool setRerun(shared_ptr<Command> c) noexcept {
    auto [iter, added] = _rerun.emplace(c);
    return added;
  }

  /// Check if command c is marked for re-execution rather than emulation
  bool checkRerun(shared_ptr<Command> c) const noexcept {
    return c && _rerun.find(c) != _rerun.end();
  }

  /// Run this build
  void run() noexcept;

  Env& getEnv() noexcept { return _env; }

  /// Commit any pending updates and save fingerprints for all final state
  void applyFinalState() noexcept;

  /****** Tracing and Emulation Methods ******/

  /// A command creates a new pipe
  shared_ptr<Pipe> pipe(shared_ptr<Command> c, shared_ptr<Pipe> emulating = nullptr) noexcept;

  /// A command creates a new symbolic link
  shared_ptr<Symlink> symlink(shared_ptr<Command> c,
                              fs::path target,
                              shared_ptr<Symlink> emulating = nullptr) noexcept;

  /// A command creates a new directory
  shared_ptr<Dir> dir(shared_ptr<Command> c,
                      mode_t mode,
                      shared_ptr<Dir> emulating = nullptr) noexcept;

  /// A command makes a reference with a path
  shared_ptr<Access> access(shared_ptr<Command> c,
                            shared_ptr<Access> base,
                            fs::path path,
                            AccessFlags flags,
                            shared_ptr<Access> emulating = nullptr) noexcept;

  /// A command accesses an artifact expecting to find a specific version
  template <class VersionType>
  void match(shared_ptr<Command> c,
             shared_ptr<Reference> ref,
             shared_ptr<VersionType> expected = nullptr,
             shared_ptr<Match<VersionType>> emulating = nullptr) noexcept;

  /// A command writes a new version to an artifact
  template <class VersionType>
  void apply(shared_ptr<Command> c,
             shared_ptr<Reference> ref,
             shared_ptr<VersionType> written = nullptr,
             shared_ptr<Apply<VersionType>> emulating = nullptr) noexcept;

  /// A command is launching a child command
  void launch(shared_ptr<Command> c,
              shared_ptr<Command> child,
              shared_ptr<Launch> emulating = nullptr) noexcept;

  /// A command is joining with a child command
  void join(shared_ptr<Command> c,
            shared_ptr<Command> child,
            int exit_status,
            shared_ptr<Join> emulating = nullptr) noexcept;

  /// Print information about this build
  ostream& print(ostream& o) const noexcept;

  friend ostream& operator<<(ostream& o, const Build& b) noexcept { return b.print(o); }

  friend ostream& operator<<(ostream& o, const Build* b) noexcept { return b->print(o); }

  /********** Observer Interface **********/

  /// Add an observer to this build
  void addObserver(shared_ptr<BuildObserver> o) noexcept { _observers.push_back(o); }

  /// Inform the observer that command c modified artifact a, creating version v
  void observeOutput(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v) noexcept {
    for (const auto& o : _observers) o->output(c, a, v);
  }

  /// Inform the observer that command c accessed version v of artifact a
  void observeInput(shared_ptr<Command> c,
                    shared_ptr<Reference> ref,
                    shared_ptr<Artifact> a,
                    shared_ptr<Version> v,
                    InputType t) noexcept;

  /// Inform the observer that command c did not find the expected version in artifact a
  /// Instead of version `expected`, the command found version `observed`
  void observeMismatch(shared_ptr<Command> c,
                       shared_ptr<Artifact> a,
                       shared_ptr<Version> observed,
                       shared_ptr<Version> expected) noexcept {
    for (const auto& o : _observers) o->mismatch(c, a, observed, expected);
  }

  /// Inform the observer that a given command's IR action would detect a change in the build env
  void observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) noexcept {
    for (const auto& o : _observers) o->commandChanged(c, s);
  }

  /// Inform observers that the version of an artifact produced during the build does not match the
  /// on-disk version.
  void observeFinalMismatch(shared_ptr<Artifact> a,
                            shared_ptr<Version> produced,
                            shared_ptr<Version> ondisk) noexcept {
    for (const auto& o : _observers) o->finalMismatch(a, produced, ondisk);
  }

 private:
  /// The trace of steps in this build
  shared_ptr<Trace> _trace;

  /// Where should the build use fingerprints for content comparisons?
  // FingerprintLevel _fingerprint;

  /// Should this build print every command as it is executed?
  bool _print_on_run;

  /// Should this build just print commands instead of running them?
  bool _dry_run;

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

  /// Access filters for version types that allow it
  AccessFilter _metadata_filter;
  AccessFilter _content_filter;

  template <class VersionType>
  AccessFilter* _getFilter() noexcept {
    return nullptr;
  }
};