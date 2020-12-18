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
    // If the version has no creator, we do not need to propagate any markings
    auto creator = v->getCreator();
    if (!creator) return;

    // If this is make accessing metadata, we only need to mark in one direction;
    // changing metadata alone does not need to trigger a re-execution of make
    if (v->as<MetadataVersion>() && c->isMake()) return;

    // If the only requirement is that the artifact exists, we don't need to create a dependency
    if (t == InputType::Exists) return;

    // Otherwise command c may have to rerun if the input's creator reruns
    _output_used_by[v->getCreator()->getCommand()].insert(c);
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
    _output_needed.insert(produced->getCreator()->getCommand());

    LOGF(rebuild, "{} must rerun: on-disk state of {} has changed (expected {}, observed {})",
         produced->getCreator()->getCommand(), a, produced, ondisk);
  }

  /// A command is being launched. The parent will be null if this is the root command.
  virtual void observeLaunch(const shared_ptr<Command>& parent,
                             const shared_ptr<Command>& child) noexcept override final {
    _commands.insert(child);
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
    for (const auto& child : c->previousRun()->getChildren()) {
      mark(child->getCommand(), RerunReason::Child, c);
    }

    // Mark any commands that produce output that this command needs
    for (const auto& [a, v, t] : c->previousRun()->getInputs()) {
      // If the version does not have a creator, there's no need to run anything to create it
      auto creator = v->getCreator();
      if (!creator) continue;

      // If the version is cached, we can commit it without running the creator
      if (options::enable_cache && v->canCommit()) continue;

      // Mark the creator for rerun so it will produce the necessary input
      mark(creator->getCommand(), RerunReason::OutputNeeded, c);
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

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;
};
