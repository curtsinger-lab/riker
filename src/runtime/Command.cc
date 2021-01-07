#include "Command.hh"

// IWYU pragma: no_include <ext/alloc_traits.h>
// IWYU pragma: no_include <utility>

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "runtime/CommandRun.hh"
#include "ui/options.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

using std::make_shared;
using std::make_unique;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

/// Get a shared pointer to the special null command instance
const shared_ptr<Command>& Command::getNullCommand() noexcept {
  static shared_ptr<Command> _null_command(new Command());
  return _null_command;
}

// Create a command
Command::Command(vector<string> args) noexcept : _args(args) {
  ASSERT(args.size() > 0) << "Attempted to create a command with no arguments";
}

// Destroy a command. The destructor has to be declared in the .cc file where we have a complete
// definition of Run
Command::~Command() noexcept = default;

// Get a short, length-limited name for this command
string Command::getShortName(size_t limit) const noexcept {
  // A command with no arguments is anonymous. This shouldn't happen, but better to be safe.
  if (_args.size() == 0) return "<anon>";

  // The first argument to the command is its name. Treat it as a path for now
  fs::path exe_path = _args.front();
  if (exe_path.is_absolute()) exe_path = exe_path.filename();

  // The output starts with the executable name
  string result = exe_path;

  // Add arguments up to the length limit
  size_t index = 1;
  while (index < _args.size() && result.length() < limit) {
    result += " " + _args[index];
    index++;
  }

  if (limit > 0 && result.length() >= limit) {
    result = result.substr(0, limit - 3) + "...";
  }

  return result;
}

// Get a full name for this command
string Command::getFullName() const noexcept {
  string result;
  bool first = true;
  for (const string& arg : _args) {
    if (!first) result += " ";
    first = false;
    result += arg;
  }
  return result;
}

// Is this command the null command?
bool Command::isNullCommand() const noexcept {
  return _args.size() == 0;
}

// Is this command a make command?
bool Command::isMake() const noexcept {
  fs::path exe_path = _args.front();
  return exe_path.filename().string() == "make";
}

// Get the command run data for the current run
const shared_ptr<CommandRun>& Command::currentRun() noexcept {
  if (!_run) _run = make_shared<CommandRun>(shared_from_this());
  return _run;
}

// Get the command run data for the previous run
const shared_ptr<CommandRun>& Command::previousRun() noexcept {
  if (!_last_run) _last_run = make_shared<CommandRun>(shared_from_this());
  return _last_run;
}

// Finish the current run and set up for another one
void Command::finishRun() noexcept {
  // Create a new instance of CommandRun in _last_run, then swap them
  _last_run = make_shared<CommandRun>(shared_from_this());
  std::swap(_run, _last_run);

  // Update the command's marking
  if (_marking == RebuildMarking::MayRun) {
    // If this is a lazy build, return MayRun to Emulate so it can be marked later.
    // If this is an eager build, MayRun commands are executed so change to AlreadyRun.
    if (options::lazy_builds) {
      _marking = RebuildMarking::Emulate;
    } else {
      _marking = RebuildMarking::AlreadyRun;
    }

  } else if (_marking == RebuildMarking::MustRun) {
    // MustRun commands were executed, so update them to AlreadyRun
    _marking = RebuildMarking::AlreadyRun;
  }

  // Emulate and AlreadyRun markings are left as-is
}

// Plan the next build based on this command's completed run
void Command::planBuild() noexcept {
  // See rebuild planning rules in docs/new-rebuild.md
  // Rules 1 & 2: If this command observe a change on its previous run, mark it for rerun
  if (previousRun()->getChanged().size() == 2) {
    if (mark(RebuildMarking::MustRun)) {
      LOGF(rebuild, "{} must run: input changed or output is missing/modified", this);
    }
  }
}

// Assign a marking to this command. Return true if the marking is new.
bool Command::mark(RebuildMarking m) noexcept {
  // See rebuild planning rules in docs/new-rebuild.md

  // Check the new marking
  if (m == RebuildMarking::MustRun) {
    // If this command already had an equivalent or higher marking, return false.
    // There's no need to propagate this marking because it is not new.
    if (_marking == RebuildMarking::MustRun || _marking == RebuildMarking::AlreadyRun) {
      return false;
    }

    // Update the marking
    _marking = RebuildMarking::MustRun;

    // Loop over metadata inputs to this command
    for (const auto& [a, v, t] : previousRun()->getMetadataInputs()) {
      // If the version does not have a creator, there's no need to run anything to create it
      auto creator = v->getCreator();
      if (!creator) continue;

      // Rule 3 does not apply, because metadata versions can always be committed

      // Rule 4: If a command D that may run produces an input to this command, mark it MustRun
      if (creator->_marking == RebuildMarking::MayRun) {
        creator->mark(RebuildMarking::MustRun);
        LOGF(rebuild, "{} must run: output is used by {}", creator, this);
      }
    }

    // Loop over content inputs to this command
    for (const auto& [a, v, t] : previousRun()->getContentInputs()) {
      // If the version does not have a creator, there's no need to run anything to create it
      auto creator = v->getCreator();
      if (!creator) continue;

      // Rule 3: Mark commands that produce uncached inputs to this command as MustRun
      // TODO: This check should really ask the artifact if it can commit the version at the time
      // of the input, not during rebuild planning.
      if (!v->canCommit()) {
        // Mark the creator for rerun so it will produce the necessary input
        if (creator->mark(RebuildMarking::MustRun)) {
          LOGF(rebuild, "{} must run: output is needed by {}", creator, this);
        }
      }

      // Rule 4: If a command D that may run produces an input to this command, mark it MustRun
      if (creator->_marking == RebuildMarking::MayRun) {
        creator->mark(RebuildMarking::MustRun);
        LOGF(rebuild, "{} must run: output is used by {}", creator, this);
      }
    }

    // Loop over the commands that use this command's outputs
    for (const auto& user : previousRun()->getOutputUsers()) {
      // Rule 5: Mark any users of this command's output as MayRun
      if (user->getCommand()->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: input may be changed by {}", user->getCommand(), this);
      }
    }

    // Rule a: Mark all of this command's children as MustRun
    for (const auto& child : previousRun()->getChildren()) {
      if (child->getCommand()->mark(RebuildMarking::MustRun)) {
        LOGF(rebuild, "{} must run: parent {} is running", child->getCommand(), this);
      }
    }

    // Rule b: If this command's parent is marked MayRun, change it to MustRun
    // TODO: Implement this once we track parents.

    // The marking was new, so return true
    return true;

  } else if (m == RebuildMarking::MayRun) {
    // If this command already had an equivalent or highe rmarking, return false.
    // There's no need to propagate this marking becasue it is not new.
    if (_marking == RebuildMarking::MayRun || _marking == RebuildMarking::MustRun ||
        _marking == RebuildMarking::AlreadyRun) {
      return false;
    }

    // Update the marking
    _marking = RebuildMarking::MayRun;

    // Loop over metadata inputs to this command
    for (const auto& [a, v, t] : previousRun()->getMetadataInputs()) {
      // If the version does not have a creator, there's no need to run anything to create it
      auto creator = v->getCreator();
      if (!creator) continue;

      // Rule 6 does not apply because metadata versions can always be committed
    }

    // Loop over content inputs to this command
    for (const auto& [a, v, t] : previousRun()->getContentInputs()) {
      // If the version does not have a creator, there's no need to run anything to create it
      auto creator = v->getCreator();
      if (!creator) continue;

      // Rule 6: Mark commands that produce uncached inputs to this command as MayRun
      // TODO: This check should really ask the artifact if it can commit the version at the time
      // of the input, not during rebuild planning.
      if (!v->canCommit()) {
        // Mark the creator for rerun so it will produce the necessary input
        if (creator->mark(RebuildMarking::MayRun)) {
          LOGF(rebuild, "{} may run: output is needed by {}", creator, this);
        }
      }
    }

    // Loop over the commands that use this command's outputs
    for (const auto& user : previousRun()->getOutputUsers()) {
      // Rule 7: Mark any users of this command's output as MayRun
      if (user->getCommand()->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: input may be changed by {}", user->getCommand(), this);
      }

      // Rule 8: If the command that uses this command's output is marked MustRun, mark this command
      // MustRun as well
      if (user->getCommand()->_marking == RebuildMarking::MustRun) {
        if (mark(RebuildMarking::MustRun)) {
          LOGF(rebuild, "{} must run: output is used by command {}", this, user->getCommand());
        }
      }
    }

    // Rule c: Mark all of this command's children as MayRun
    for (const auto& child : previousRun()->getChildren()) {
      if (child->getCommand()->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: parent {} may run", child->getCommand(), this);
      }
    }

    // The marking was new, so return true
    return true;

  } else {
    // Emulate and AlreadyRun are never new markings
    return false;
  }
}

// Check if this command must rerun
bool Command::mustRerun() const noexcept {
  // Commands marked MustRun have to run
  if (_marking == RebuildMarking::MustRun) return true;

  // Commands marked MayRun have to run if this is not a lazy build
  if (!options::lazy_builds && _marking == RebuildMarking::MayRun) return true;

  // Otherwise the command does not need to run
  return false;
}
