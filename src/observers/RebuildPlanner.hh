#pragma once

#include <map>
#include <memory>
#include <set>

#include "artifacts/Artifact.hh"
#include "build/Command.hh"
#include "build/RebuildPlan.hh"
#include "interfaces/BuildObserver.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "versions/MetadataVersion.hh"

using std::map;
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

  /// Create a rebuild plan
  RebuildPlan planBuild() const noexcept {
    RebuildPlan plan;

    // Mark all the commands with changed inputs
    for (const auto& c : _changed) {
      mark(plan, c, Reason::Changed);
    }

    // Mark all the commands whose output is required
    for (const auto& c : _output_needed) {
      mark(plan, c, Reason::OutputNeeded);
    }

    return plan;
  }

  /// Get the set of commands that directly observe a change
  const set<shared_ptr<Command>>& getChanged() const noexcept { return _changed; }

  /// Get the set of commands whose output is needed
  const set<shared_ptr<Command>>& getOutputNeeded() const noexcept { return _output_needed; }

  /******** BuildObserver Interface ********/

  /// Command c depends on version v of artifact a
  virtual void input(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v,
                     InputType t) noexcept override final {
    // During the planning phase, record this dependency
    if (v->getCreator()) {
      // Output from creator is used by c. If creator reruns, c may have to rerun.
      // This is not true for inputs that just require the version to exist
      if (t != InputType::Exists) {
        _output_used_by[v->getCreator()].insert(c);
      }

      // The dependency back edge depends on caching
      if (options::enable_cache && a->canCommit(v)) {
        // If the requested artifact can commit the version we need, there's no need to depend on
        // the creator of this version.

      } else {
        // Otherwise, if c has to run then we also need to run creator to produce this input
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
    LOG(rebuild) << c << " observed change in " << a << " version " << observed << ", expected "
                 << expected;
    _changed.insert(c);
  }

  /// Command c has never been run
  virtual void commandNeverRun(shared_ptr<Command> c) noexcept override final {
    // Record the change
    LOG(rebuild) << c << " has never run";
    _changed.insert(c);
  }

  /// IR step s in command c observed a change
  virtual void commandChanged(shared_ptr<Command> c) noexcept override final {
    // Record the change
    LOG(rebuild) << c << " observed change in emulated step";
    _changed.insert(c);
  }

  /// An artifact's final version does not match what is on the filesystem
  virtual void finalMismatch(shared_ptr<Artifact> a,
                             shared_ptr<Version> produced,
                             shared_ptr<Version> ondisk) noexcept override final {
    // If this artifact was not created by any command, there's nothing we can do about it
    if (!produced->getCreator()) return;

    // If this artifact is cached, we can just stage it in
    if (options::enable_cache && a->canCommit(produced)) return;

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
  void mark(RebuildPlan& plan,
            shared_ptr<Command> c,
            Reason reason,
            shared_ptr<Command> prev = nullptr) const noexcept {
    // Mark the command for the given reason. If it was already marked, return
    if (!plan.mark(c, reason)) return;

    if (reason == Reason::Changed) {
      LOG(rebuild) << c << " must rerun because it directly observed a change";

    } else if (reason == Reason::Child) {
      ASSERT(prev) << "Invalid call to mark without previous command";
      LOG(rebuild) << c << " must rerun because its parent, " << prev << ", is rerunning";

    } else if (reason == Reason::InputMayChange) {
      ASSERT(prev) << "Invalid call to mark without previous command";
      LOG(rebuild) << c << " must rerun because its input may be changed by " << prev;

    } else if (reason == Reason::OutputNeeded) {
      if (prev) {
        LOG(rebuild) << c << " must rerun because its unsaved output is needed by " << prev;
      } else {
        LOG(rebuild) << c << " must rerun because its unsaved output is changed or missing";
      }
    }

    // Mark this command's children
    if (auto iter = _children.find(c); iter != _children.end()) {
      for (const auto& child : iter->second) {
        mark(plan, child, Reason::Child, c);
      }
    }

    // Mark any commands that produce output that this command needs
    if (auto iter = _needs_output_from.find(c); iter != _needs_output_from.end()) {
      for (const auto& other : iter->second) {
        mark(plan, other, Reason::OutputNeeded, c);
      }
    }

    // Mark any commands that use this command's output
    if (auto iter = _output_used_by.find(c); iter != _output_used_by.end()) {
      for (const auto& other : iter->second) {
        mark(plan, other, Reason::InputMayChange, c);
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
