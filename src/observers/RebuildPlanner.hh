#pragma once

#include <map>
#include <memory>
#include <set>

#include "artifacts/Artifact.hh"
#include "interfaces/BuildObserver.hh"
#include "runtime/Command.hh"
#include "runtime/RebuildPlan.hh"
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
    for (auto c : getChanged()) {
      mark(plan, c, Reason::Changed);
    }

    // Mark all the commands whose output is required
    for (const auto& c : _output_needed) {
      mark(plan, c, Reason::OutputNeeded);
    }

    return plan;
  }

  /// Get the set of commands that directly observe a change
  set<shared_ptr<Command>> getChanged() const noexcept {
    set<shared_ptr<Command>> changed;
    std::set_intersection(_changed_build.begin(), _changed_build.end(), _changed_post_build.begin(),
                          _changed_post_build.end(), std::inserter(changed, changed.begin()));
    return changed;
  }

  /// Get the set of commands whose output is needed
  const set<shared_ptr<Command>>& getOutputNeeded() const noexcept { return _output_needed; }

  /******** BuildObserver Interface ********/

  /// Command c depends on version v of artifact a
  virtual void observeInput(shared_ptr<Command> c,
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
  virtual void observeMismatch(shared_ptr<Command> c,
                               Scenario scenario,
                               shared_ptr<Artifact> a,
                               shared_ptr<Version> observed,
                               shared_ptr<Version> expected) noexcept override final {
    // Record the change
    LOGF(rebuild, "{} changed in scenario {}: change in {} (expected {}, observed {})", c, scenario,
         a, expected, observed);

    if (scenario == Scenario::Build) {
      _changed_build.insert(c);
    } else if (scenario == Scenario::PostBuild) {
      _changed_post_build.insert(c);
    } else {
      _changed_build.insert(c);
      _changed_post_build.insert(c);
    }
  }

  /// Command c has never been run
  virtual void observeCommandNeverRun(shared_ptr<Command> c) noexcept override final {
    LOGF(rebuild, "{} changed: never run", c);
    _changed_build.insert(c);
    _changed_post_build.insert(c);
  }

  /// A command's reference did not resolve as expected
  virtual void observeResolutionChange(shared_ptr<Command> c,
                                       Scenario scenario,
                                       shared_ptr<Ref> ref,
                                       int expected) noexcept override {
    LOGF(rebuild,
         "{} changed in scenario {}: {} did not resolve as expected (expected {}, observed {})", c,
         scenario, ref, expected, ref->getResultCode());
    if (scenario == Scenario::Build) {
      _changed_build.insert(c);
    } else if (scenario == Scenario::PostBuild) {
      _changed_post_build.insert(c);
    } else {
      _changed_build.insert(c);
      _changed_post_build.insert(c);
    }
  }

  /// Two references did not compare as expected
  virtual void observeRefMismatch(shared_ptr<Command> c,
                                  shared_ptr<Ref> ref1,
                                  shared_ptr<Ref> ref2,
                                  RefComparison type) noexcept override {
    LOGF(rebuild, "{} changed: {} and {} did not compare as expected", c, ref1, ref2);
    _changed_build.insert(c);
    _changed_post_build.insert(c);
  }

  /// A child command did not exit with the expected status
  virtual void observeExitCodeChange(shared_ptr<Command> parent,
                                     shared_ptr<Command> child,
                                     int expected,
                                     int observed) noexcept override {
    LOGF(rebuild, "{} changed: child {} exited with different status (expected {}, observed {})",
         parent, child, expected, observed);
    _changed_build.insert(parent);
    _changed_post_build.insert(parent);
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
  virtual void observeLaunch(shared_ptr<Command> parent,
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
      // The change has already been logged

    } else if (reason == Reason::Child) {
      ASSERT(prev) << "Invalid call to mark without previous command";
      LOGF(rebuild, "{} must run: parent command {} is rerunning", c, prev);

    } else if (reason == Reason::InputMayChange) {
      ASSERT(prev) << "Invalid call to mark without previous command";
      LOGF(rebuild, "{} must rerun: input may be changed by {}", c, prev);

    } else if (reason == Reason::OutputNeeded) {
      if (prev) {
        LOGF(rebuild, "{} must rerun: unsaved output is needed by {}", c, prev);
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

  /// Commands that have changed since the last build
  set<shared_ptr<Command>> _changed_build;

  /// Commands that have changed since their post-build state
  set<shared_ptr<Command>> _changed_post_build;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Map command that consumes uncached input -> commands that produce that input
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;
};
