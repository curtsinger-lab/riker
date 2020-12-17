#include "Command.hh"

#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "runtime/Run.hh"
#include "versions/Version.hh"

using std::cout;
using std::endl;
using std::make_shared;
using std::make_unique;
using std::map;
using std::nullopt;
using std::set;
using std::shared_ptr;
using std::string;

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
const shared_ptr<Run>& Command::currentRun() noexcept {
  if (!_run) _run = make_shared<Run>(shared_from_this());
  return _run;
}

// Get the command run data for the previous run
const shared_ptr<Run>& Command::previousRun() noexcept {
  if (!_last_run) _last_run = make_shared<Run>(shared_from_this());
  return _last_run;
}

// Finish the current run and set up for another one
void Command::finishRun() noexcept {
  // Create a new Run struct in _last_run, then swap them
  _last_run = make_shared<Run>(shared_from_this());
  std::swap(_run, _last_run);
}
