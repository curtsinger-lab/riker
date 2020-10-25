#pragma once

#include <iostream>
#include <map>
#include <memory>

#include "runtime/Command.hh"
#include "ui/options.hh"

using std::endl;
using std::map;
using std::shared_ptr;

/// Record the reason why a command has been marked for rerun. Reasons are ordered; any command
/// marked with both Child and Changed will retain the Changed marking.
enum class Reason : int {
  Child = 0,           // The marked command is a child of another command marked for rerun
  InputMayChange = 1,  // The marked command consumes output from another command marked for rerun
  OutputNeeded = 2,    // The marked command produces output needed by another marked command
  Changed = 3          // The marked command directly observed a change
};

class RebuildPlan {
 public:
  /// Create an empty rebuild plan, which will emulate all commands by default
  RebuildPlan() noexcept = default;

  /// Check if a command can be emulated
  bool canEmulate(const shared_ptr<Command>& c) const noexcept {
    // Look for the provided command. If it is not marked, it can be emulated
    return _plan.find(c) == _plan.end();
  }

  /// Check if a command can be skipped
  bool canSkip(const shared_ptr<Command>& c) const noexcept {
    // Look for the provided command. If it is not found, or is marked only as a child, it can be
    // skipped
    auto iter = _plan.find(c);
    return iter == _plan.end() || iter->second == Reason::Child;
  }

  /// Check if a command must be rerun
  bool mustRerun(const shared_ptr<Command>& c) const noexcept {
    return !canEmulate(c) && !canSkip(c);
  }

  /**
   * Mark a command in the rebuild plan. The command will always retain its highest-level marking
   * \param c       The command being marked
   * \param reason  The reason the command is being marked
   * \returns true if the marking is new, meaning the command was previously unmarked
   */
  bool mark(const shared_ptr<Command>& c, Reason reason) noexcept {
    // Try to add the marking to the plan
    auto [iter, added] = _plan.emplace(c, reason);

    // If the marking was not new, make sure higher of the two markings is retained
    if (!added && iter->second < reason) iter->second = reason;

    // Return true if the marking is new
    return added;
  }

  friend ostream& operator<<(ostream& o, const RebuildPlan& r) noexcept {
    if (r._plan.size() > 0) {
      o << "The following commands will be rerun:" << endl;
      for (const auto& [c, reason] : r._plan) {
        o << "  " << c->getShortName(options::command_length) << endl;
      }

    } else {
      o << "No commands to rerun" << endl;
    }

    return o;
  }

 private:
  /// Keep track of the planned re-execution mode for each known command
  map<shared_ptr<Command>, Reason> _plan;
};
