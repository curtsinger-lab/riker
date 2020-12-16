#pragma once

#include <map>
#include <memory>
#include <set>

#include "artifacts/Artifact.hh"
#include "interfaces/BuildObserver.hh"
#include "runtime/Command.hh"
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
  void planBuild() const noexcept {
    // Mark all the commands with changed inputs
    for (auto c : getChanged()) {
      mark(c, RerunReason::Changed);
    }

    // Mark all the commands whose output is required
    for (const auto& c : _output_needed) {
      mark(c, RerunReason::OutputNeeded);
    }
  }

  /// Get the set of commands that directly observe a change
  set<shared_ptr<Command>> getChanged() const noexcept {
    set<shared_ptr<Command>> changed;

    for (auto c : _commands) {
      if (c->previousRun()->getChanged().size() == 2) {
        changed.insert(c);
      }
    }

    return changed;
  }

  /// Get the set of commands whose output is needed
  const set<shared_ptr<Command>>& getOutputNeeded() const noexcept { return _output_needed; }

  /******** BuildObserver Interface ********/

  /// Command c depends on version v of artifact a
  virtual void observeInput(const shared_ptr<Command>& c,
                            shared_ptr<Artifact> a,
                            shared_ptr<Version> v,
                            InputType t) noexcept override final {
    // During the planning phase, record this dependency
    if (v->getCreator()) {
      // Special case: make will stat files at the end of the build. We'll skip recording these
      // inputs for now. If we don't do that, any build that updates a target will force a full
      // rebuild with make.
      if (auto metadata = v->as<MetadataVersion>(); metadata && c->isMake()) {
        // TODO: Revisit this special case once we have command skipping. We could defer running
        // make but then launch is later (and skip its children) if the stat data changes.

        // make needs the output from the creator if we need to rerun it, but don't add make to the
        // creator's set of commands that it will mark for rerun
        _needs_output_from[c].insert(v->getCreator());
        return;
      }

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
  virtual void observeMismatch(const shared_ptr<Command>& c,
                               Scenario scenario,
                               shared_ptr<Artifact> a,
                               shared_ptr<Version> observed,
                               shared_ptr<Version> expected) noexcept override final {
    // Record the change
    LOGF(rebuild, "{} changed in scenario {}: change in {} (expected {}, observed {})", c, scenario,
         a, expected, observed);

    c->currentRun()->observeChange(scenario);
  }

  /// Command c has never been run
  virtual void observeCommandNeverRun(const shared_ptr<Command>& c) noexcept override final {
    LOGF(rebuild, "{} changed: never run", c);
    c->currentRun()->observeChange(Scenario::Build);
    c->currentRun()->observeChange(Scenario::PostBuild);
  }

  /// Two references did not compare as expected
  virtual void observeRefMismatch(const shared_ptr<Command>& c,
                                  shared_ptr<Ref> ref1,
                                  shared_ptr<Ref> ref2,
                                  RefComparison type) noexcept override {
    LOGF(rebuild, "{} changed: {} and {} did not compare as expected", c, ref1, ref2);

    c->currentRun()->observeChange(Scenario::Build);
    c->currentRun()->observeChange(Scenario::PostBuild);
  }

  /// A child command did not exit with the expected status
  virtual void observeExitCodeChange(const shared_ptr<Command>& parent,
                                     const shared_ptr<Command>& child,
                                     int expected,
                                     int observed) noexcept override {
    LOGF(rebuild, "{} changed: child {} exited with different status (expected {}, observed {})",
         parent, child, expected, observed);

    parent->currentRun()->observeChange(Scenario::Build);
    parent->currentRun()->observeChange(Scenario::PostBuild);
  }

  /// An artifact's final version does not match what is on the filesystem
  virtual void observeFinalMismatch(shared_ptr<Artifact> a,
                                    shared_ptr<Version> produced,
                                    shared_ptr<Version> ondisk) noexcept override final {
    // If this artifact was not created by any command, there's nothing we can do about it
    if (!produced->getCreator()) return;

    // If this artifact is cached, we can just stage it in
    if (options::enable_cache && a->canCommit(produced)) return;

    // Otherwise we have to run the command that created this artifact
    _output_needed.insert(produced->getCreator());

    LOGF(rebuild, "{} must rerun: on-disk state of {} has changed (expected {}, observed {})",
         produced->getCreator(), a, produced, ondisk);
  }

  /// A command is being launched. The parent will be null if this is the root command.
  virtual void observeLaunch(const shared_ptr<Command>& parent,
                             const shared_ptr<Command>& child) noexcept override final {
    _commands.insert(child);

    if (parent) _children[parent].insert(child);
  }

 private:
  /// Mark a command for rerun, and propagate that marking to its dependencies/dependents
  void mark(const shared_ptr<Command>& c,
            RerunReason reason,
            const shared_ptr<Command>& prev = nullptr) const noexcept {
    // Mark the command for the given reason. If it was already marked, return
    if (!c->previousRun()->markForRerun(reason)) return;

    if (reason == RerunReason::Changed) {
      // The change has already been logged

    } else if (reason == RerunReason::Child) {
      ASSERT(prev) << "Invalid call to mark without previous command";
      LOGF(rebuild, "{} must run: parent command {} is rerunning", c, prev);

    } else if (reason == RerunReason::InputMayChange) {
      ASSERT(prev) << "Invalid call to mark without previous command";
      LOGF(rebuild, "{} must rerun: input may be changed by {}", c, prev);

    } else if (reason == RerunReason::OutputNeeded) {
      if (prev) {
        LOGF(rebuild, "{} must rerun: unsaved output is needed by {}", c, prev);
      }
    }

    // Mark this command's children
    if (auto iter = _children.find(c); iter != _children.end()) {
      for (const auto& child : iter->second) {
        mark(child, RerunReason::Child, c);
      }
    }

    // Mark any commands that produce output that this command needs
    if (auto iter = _needs_output_from.find(c); iter != _needs_output_from.end()) {
      for (const auto& other : iter->second) {
        mark(other, RerunReason::OutputNeeded, c);
      }
    }

    // Mark any commands that use this command's output
    if (auto iter = _output_used_by.find(c); iter != _output_used_by.end()) {
      for (const auto& other : iter->second) {
        mark(other, RerunReason::InputMayChange, c);
      }
    }
  }

 private:
  /// Track all commands
  set<shared_ptr<Command>> _commands;

  /// Track each command's children
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _children;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Map command that consumes uncached input -> commands that produce that input
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;
};
