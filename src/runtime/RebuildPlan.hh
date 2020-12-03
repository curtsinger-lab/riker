#pragma once

#include <iostream>
#include <map>
#include <memory>

#include "runtime/Command.hh"
#include "ui/options.hh"

using std::endl;
using std::map;
using std::shared_ptr;

class RebuildPlan {
 public:
  /// Create an empty rebuild plan, which will emulate all commands by default
  RebuildPlan() noexcept = default;

  /// Check if a command must be rerun
  bool mustRerun(const shared_ptr<Command>& c) const noexcept { return c->mustRerun(); }

  /**
   * Mark a command in the rebuild plan. The command will always retain its highest-level marking
   * \param c       The command being marked
   * \param reason  The reason the command is being marked
   * \returns true if the marking is new, meaning the command was previously unmarked
   */
  bool mark(const shared_ptr<Command>& c, RerunReason reason) noexcept {
    return c->markForRerun(reason);
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
  map<shared_ptr<Command>, RerunReason> _plan;
};
