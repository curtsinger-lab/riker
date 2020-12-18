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
    for (const auto& c : _commands) {
      c->previousRun()->planBuild();
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
  /// Track all commands
  set<shared_ptr<Command>> _commands;
};
