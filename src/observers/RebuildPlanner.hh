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

  /******** BuildObserver Interface ********/

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
    for (const auto& user : c->previousRun()->getOutputUsers()) {
      mark(user->getCommand(), RerunReason::InputMayChange, c);
    }
  }

 private:
  /// Track all commands
  set<shared_ptr<Command>> _commands;
};
