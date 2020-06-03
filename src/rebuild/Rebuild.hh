#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>

#include "data/AccessFlags.hh"
#include "data/Command.hh"
#include "data/InitialFD.hh"
#include "rebuild/Artifact.hh"
#include "rebuild/BuildObserver.hh"
#include "rebuild/Env.hh"
#include "tracing/Tracer.hh"

using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;

class Access;
class Reference;
class Pipe;

/// This class captures all of the logic and state required to plan a rebuild.
class Rebuild : public BuildObserver {
 public:
  /// Create a rebuild planner
  Rebuild() = default;

  // Disallow Copy
  Rebuild(const Rebuild&) = delete;
  Rebuild& operator=(const Rebuild&) = delete;

  // Allow Move
  Rebuild(Rebuild&&) = default;
  Rebuild& operator=(Rebuild&&) = default;

  /**
   * Identify the commands that must rerun and mark them in the given Build
   * \param b The build that will be used to execute any commands this rebuild marks for rerun
   */
  void planBuild(Build& b) const;

  /// Print information about the rebuild state
  ostream& print(ostream& o) const;

  /// Print a Rebuild
  friend ostream& operator<<(ostream& o, const Rebuild& r) { return r.print(o); }

  /// Print a Rebuild pointer
  friend ostream& operator<<(ostream& o, const Rebuild* r) { return r->print(o); }

  /******** BuildObserver Interface ********/

  /// Command c depends on the metadata for artifact a
  virtual void metadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) override;

  /// Command c depends on the contents of artifact a
  virtual void contentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) override;

  /// Command c did not find the expected metadata in artifact a
  virtual void metadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) override;

  /// Command c did not find the expected metadata in artifact a
  virtual void contentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) override;

  /// Command c has never been run
  virtual void commandNeverRun(shared_ptr<Command> c) override;

  /// IR step s in command c observed a change
  virtual void commandChanged(shared_ptr<Command> c, shared_ptr<const Step> s) override;

  /// An artifact's final contents do not match what is on the filesystem
  virtual void finalContentMismatch(shared_ptr<Artifact> a) override;

 private:
  /// Mark a command for rerun, and propagate that marking to its dependencies/dependents
  void mark(Build& build, shared_ptr<Command> c) const;

 private:
  /// Track commands with changed inputs
  set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Map command that consumes uncached input -> commands that produce that input
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;
};
