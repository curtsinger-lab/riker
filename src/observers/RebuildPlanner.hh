#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>

#include "artifacts/Artifact.hh"
#include "build/BuildObserver.hh"
#include "core/Command.hh"
#include "ui/options.hh"
#include "util/log.hh"

using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;

/// This class captures all of the logic and state required to plan a rebuild.
class RebuildPlanner final : public BuildObserver {
 public:
  /// Create a rebuild planner
  RebuildPlanner() noexcept = default;

  // Disallow Copy
  RebuildPlanner(const RebuildPlanner&) = delete;
  RebuildPlanner& operator=(const RebuildPlanner&) = delete;

  // Allow Move
  RebuildPlanner(RebuildPlanner&&) noexcept = default;
  RebuildPlanner& operator=(RebuildPlanner&&) noexcept = default;

  /**
   * Identify the commands that must rerun and mark them in the given Build
   * \param b The build that will be used to execute any commands this rebuild marks for rerun
   */
  void planBuild(Build& b) const noexcept {
    // Mark all the commands with changed inputs
    for (const auto& c : _changed) {
      mark(b, c);
    }

    // Mark all the commands whose output is required
    for (const auto& c : _output_needed) {
      mark(b, c);
    }
  }

  /// Print information about the rebuild state
  ostream& print(ostream& o) const noexcept {
    if (_changed.size() > 0) {
      o << "Commands with changed inputs:" << endl;
      for (const auto& c : _changed) {
        o << "  " << c->getShortName(options::command_length) << endl;
      }
      o << endl;
    }

    if (_output_needed.size() > 0) {
      o << "Commands whose output is missing or modified:" << endl;
      for (const auto& c : _output_needed) {
        o << "  " << c->getShortName(options::command_length) << endl;
      }
      o << endl;
    }

    return o;
  }

  /// Print a RebuildPlanner reference
  friend ostream& operator<<(ostream& o, const RebuildPlanner& r) noexcept { return r.print(o); }

  /// Print a RebuildPlanner pointer
  friend ostream& operator<<(ostream& o, const RebuildPlanner* r) noexcept { return r->print(o); }

  /******** BuildObserver Interface ********/

  /// Command c depends on version v of artifact a
  virtual void input(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v,
                     InputType t) noexcept override final {
    // During the planning phase, record this dependency
    if (v->getCreator()) {
      // Output from creator is used by c. If creator reruns, c may have to rerun.
      _output_used_by[v->getCreator()].insert(c);

      // The dependency back edge depends on caching
      if (options::enable_cache && a->isSaved()) {
        // If this artifact is cached, we could restore it before c runs.
      } else {
        // Otherwise, if c has to run then we also need to run creator to produce this input
        INFO << c << " needs unsaved output " << a << " version " << v << " from "
             << v->getCreator();
        _needs_output_from[c].insert(v->getCreator());
      }
    }
  }

  /// Command c did not find the expected version in artifact a
  virtual void mismatch(shared_ptr<Command> c,
                        shared_ptr<Artifact> a,
                        shared_ptr<Version> observed,
                        shared_ptr<Version> expected) noexcept override final {
    // Record the change
    LOG << c << " observed change in " << a << " version " << observed << ", expected " << expected;
    _changed.insert(c);
  }

  /// Command c has never been run
  virtual void commandNeverRun(shared_ptr<Command> c) noexcept override final {
    // Record the change
    LOG << c << " never run";
    _changed.insert(c);
  }

  /// IR step s in command c observed a change
  virtual void commandChanged(shared_ptr<Command> c,
                              shared_ptr<const Step> s) noexcept override final {
    // Record the change
    LOG << c << " changed: " << s;
    _changed.insert(c);
  }

  /// An artifact's final version does not match what is on the filesystem
  virtual void finalMismatch(shared_ptr<Artifact> a,
                             shared_ptr<Version> produced,
                             shared_ptr<Version> ondisk) noexcept override final {
    // If this artifact was not created by any command, there's nothing we can do about it
    if (!produced->getCreator()) return;

    // If this artifact is cached, we can just stage it in
    if (options::enable_cache && a->isSaved()) return;

    // Otherwise we have to run the command that created this artifact
    _output_needed.insert(produced->getCreator());
  }

  /// A command is being launched. The parent will be null if this is the root command.
  virtual void launch(shared_ptr<Command> parent,
                      shared_ptr<Command> child) noexcept override final {
    if (parent) _children[parent].insert(child);
  }

 private:
  /// Mark a command for rerun, and propagate that marking to its dependencies/dependents
  void mark(Build& b, shared_ptr<Command> c) const noexcept {
    // Mark command c for rerun. If the command was already marked, setRerun will return false and
    // we can stop here
    if (!b.setRerun(c)) return;

    // Mark this command's children
    if (auto iter = _children.find(c); iter != _children.end()) {
      for (const auto& child : iter->second) {
        mark(b, child);
      }
    }

    // Mark any commands that produce output that this command needs
    if (auto iter = _needs_output_from.find(c); iter != _needs_output_from.end()) {
      for (const auto& other : iter->second) {
        mark(b, other);
      }
    }

    // Mark any commands that use this command's output
    if (auto iter = _output_used_by.find(c); iter != _output_used_by.end()) {
      for (const auto& other : iter->second) {
        mark(b, other);
      }
    }
  }

 private:
  /// Track each command's children
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _children;

  /// Track commands with changed inputs
  set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Map command that consumes uncached input -> commands that produce that input
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;
};
