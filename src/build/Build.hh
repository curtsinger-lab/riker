#pragma once

#include <memory>
#include <ostream>
#include <set>
#include <vector>

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
  Build(shared_ptr<Trace> trace) noexcept : _trace(trace), _env(*this), _tracer(*this) {}

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

  /****** Tracing and Emulation Methods ******/

  /// A command is creating a pipe
  shared_ptr<Pipe> pipe(shared_ptr<Command> c, shared_ptr<Pipe> emulating = nullptr) noexcept;

  /// A command makes a reference with a path
  shared_ptr<Access> access(shared_ptr<Command> c,
                            shared_ptr<Access> base,
                            fs::path path,
                            AccessFlags flags,
                            shared_ptr<Access> emulating = nullptr) noexcept;

  /// A command accesses an artifact's metadata
  void metadataMatch(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<MetadataVersion> expected = nullptr,
                     shared_ptr<MetadataMatch> emulating = nullptr) noexcept;

  /// A command modifies an artifact's metadata
  void setMetadata(shared_ptr<Command> c,
                   shared_ptr<Reference> ref,
                   shared_ptr<MetadataVersion> written = nullptr,
                   shared_ptr<SetMetadata> emulating = nullptr) noexcept;

  /// A command accesses an artifact's contents
  void contentsMatch(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<ContentVersion> expected = nullptr,
                     shared_ptr<ContentsMatch> emulating = nullptr) noexcept;

  /// A command modifies an artifact's contents
  void setContents(shared_ptr<Command> c,
                   shared_ptr<Reference> ref,
                   shared_ptr<ContentVersion> written = nullptr,
                   shared_ptr<SetContents> emulating = nullptr) noexcept;

  /// An emulated command modifies an artifact's contents
  void emulateSetContents(shared_ptr<Command> c, shared_ptr<SetContents> step) noexcept;

  /// A traced command accesses the contents of a symlink
  void traceSymlinkMatch(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept;

  /// An emulated command accesses the contents of a symlink
  void emulateSymlinkMatch(shared_ptr<Command> c, shared_ptr<SymlinkMatch> step) noexcept;

  /// A traced command adds an entry to a directory
  void traceLink(shared_ptr<Command> c,
                 shared_ptr<Reference> ref,
                 string entry,
                 shared_ptr<Reference> target) noexcept;

  /// An emulated command adds an entry to a directory
  void emulateLink(shared_ptr<Command> c, shared_ptr<Link> step) noexcept;

  /// A traced command removes an entry from a directory
  void traceUnlink(shared_ptr<Command> c, shared_ptr<Reference> ref, string entry) noexcept;

  /// An emulated command removes an entry from a directory
  void emulateUnlink(shared_ptr<Command> c, shared_ptr<Unlink> step) noexcept;

  /// A traced command launches a child command
  void traceLaunch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept;

  /// An emulated command launches a child command
  void emulateLaunch(shared_ptr<Command> c, shared_ptr<Launch> step) noexcept;

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
                    shared_ptr<Artifact> a,
                    shared_ptr<Version> v,
                    InputType t) noexcept {
    for (const auto& o : _observers) o->input(c, a, v, t);
  }

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

  /// Inform observers that an artifact's version does not match the expected final state
  void observeFinalMismatch(shared_ptr<Artifact> a,
                            shared_ptr<Version> observed,
                            shared_ptr<Version> expected) noexcept {
    for (const auto& o : _observers) o->finalMismatch(a, observed, expected);
  }

 private:
  /// The trace of steps in this build
  shared_ptr<Trace> _trace;

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