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

// Create an empty command
Command::Command() noexcept : _run(make_unique<RunData>()) {}

// Create a command
Command::Command(vector<string> args) noexcept : _args(args), _run(make_unique<RunData>()) {
  ASSERT(args.size() > 0) << "Attempted to create a command with no arguments";
}

// Destroy a command. The destructor has to be declared in the .cc file where we have a complete
// definition of Command::RunData
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

/**
 * The RunData struct tracks transient data associated with a specific run of a command. That run
 * can be either an emulation or traced execution. Data from the previous run is used to match a
 * traced command's behavior against its previous emulated run, e.g. when matching children
 * launched by a traced command against its old children from the previous trace.
 */
struct Command::RunData {
  /// The command's local references
  vector<shared_ptr<Ref>> refs;

  /// This command's use countn for each of its references
  vector<size_t> refs_use_count;

  /// The children launched by this command
  list<shared_ptr<Command>> children;

  /// The exit status for this command
  int exit_status = -1;

  /// If this command is marked for re-execution, the optional will have a value
  optional<RerunReason> rerun_reason;

  /// The set of inputs to this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>, InputType>> inputs;

  /// The set of outputs from this command
  set<tuple<shared_ptr<Artifact>, shared_ptr<Version>>> outputs;
};

// Finish the current run and set up for another one
void Command::finishRun() noexcept {
  // Move current run data over to the last run
  _last_run = std::move(_run);

  // Create a new RunData struct to track current run state
  _run = make_unique<RunData>();
}

// Prepare this command to execute by creating dependencies and committing state
void Command::createLaunchDependencies(Build& build) noexcept {
  for (Command::RefID id = 0; id < _run->refs.size(); id++) {
    const auto& ref = _run->refs[id];

    // Is the ref assigned? If not, skip ahead
    if (!ref) continue;

    if (id == Command::CwdRef) {
      // The current directory has to exist to launch the command
      ref->getArtifact()->mustExist(build, shared_from_this());

    } else {
      // All other referenced artifacts must be fully committed, except we'll ignore pipes for now
      if (ref->getArtifact()->as<PipeArtifact>()) continue;

      if (ref->getArtifact()->canCommitAll()) {
        ref->getArtifact()->commitAll();
      } else {
        WARN << "Launching " << this << " without committing referenced artifact "
             << ref->getArtifact();
      }
    }
  }
}

// Get a reference from this command's reference table
const shared_ptr<Ref>& Command::getRef(Command::RefID id) const noexcept {
  ASSERT(id >= 0 && id < _run->refs.size()) << "Invalid reference ID " << id << " in " << this;
  ASSERT(_run->refs[id]) << "Access to null reference ID " << id << " in " << this;
  return _run->refs[id];
}

// Store a reference at a known index of this command's local reference table
void Command::setRef(Command::RefID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= _run->refs.size()) _run->refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  // ASSERT(!_run->refs[id]) << "Attempted to overwrite reference ID " << id << " in " << this;

  // Save the ref
  _run->refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Command::RefID Command::setRef(shared_ptr<Ref> ref) noexcept {
  RefID id = _run->refs.size();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;
  _run->refs.push_back(ref);

  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool Command::usingRef(Command::RefID id) noexcept {
  ASSERT(id >= 0 && id < _run->refs.size()) << "Invalid ref ID " << id << " in " << this;

  // Expand the use count vector if necessary
  if (_run->refs_use_count.size() <= id) _run->refs_use_count.resize(id + 1);

  // Increment the ref count. Is this the first use of the ref?
  if (_run->refs_use_count[id]++ == 0) {
    // This was the first use. Increment the user count in the ref, and return true
    _run->refs[id]->addUser();
    return true;
  }

  return false;
}

// Decrement this command's use counter for a Ref.
// Return true if that was the last use by this command.
bool Command::doneWithRef(Command::RefID id) noexcept {
  ASSERT(id >= 0 && id < _run->refs.size()) << "Invalid ref ID " << id << " in " << this;
  ASSERT(id < _run->refs_use_count.size() && _run->refs_use_count[id] > 0)
      << "Attempted to end an unknown use of ref r" << id << " in " << this;

  // Decrement the ref count. Was this the last use of the ref?
  if (--_run->refs_use_count[id] == 0) {
    // This was the last use. Decrement the user count in the ref and return true
    _run->refs[id]->removeUser();
    return true;
  }

  return false;
}

// Get this command's exit status
int Command::getExitStatus() const noexcept {
  return _run->exit_status;
}

// Set this command's exit status, and record that it has exited
void Command::setExitStatus(int status) noexcept {
  _run->exit_status = status;
}

// Record that this command launched a child command
void Command::addChild(shared_ptr<Command> child) noexcept {
  _run->children.push_back(child);
}

// Get this command's children
const list<shared_ptr<Command>>& Command::getChildren() const noexcept {
  return _last_run->children;
}

// Look for a command that matches one of this command's children from the last run
shared_ptr<Command> Command::findChild(vector<string> args,
                                       Command::RefID exe_ref,
                                       Command::RefID cwd_ref,
                                       Command::RefID root_ref,
                                       map<int, Command::RefID> fds) noexcept {
  // If there is no data from the last run, there are no children to match against
  if (!_last_run) return nullptr;

  // Loop over this command's children from the last run
  for (auto iter = _last_run->children.begin(); iter != _last_run->children.end(); iter++) {
    const auto& child = *iter;

    // Does the child match the given launch parameters?
    // TODO: Check more than just arguments
    if (child->getArguments() == args) {
      // Removed the child from the list so it cannot be matched again
      _last_run->children.erase(iter);
      return child;
    }
  }

  // No match found
  return nullptr;
}

// Mark this command for re-execution
bool Command::markForRerun(RerunReason reason) noexcept {
  // Is this command already marked?
  bool already_marked = _last_run->rerun_reason.has_value();

  // If not, or if the given reason is "higher" than the previous marking, update it
  if (!already_marked || reason > _last_run->rerun_reason.value()) {
    _last_run->rerun_reason = reason;
  }

  // Return true if this was a new marking
  return !already_marked;
}

// Check to see if this command was marked for re-execution after the last run
bool Command::mustRerun() const noexcept {
  // If there is no previous run, all commands should be emulated
  if (!_last_run) return false;

  // Otherwise check the last run state
  return _last_run->rerun_reason.has_value();
}

// Add an input to this command
void Command::addInput(shared_ptr<Artifact> a, shared_ptr<Version> v, InputType t) noexcept {
  _run->inputs.emplace(a, v, t);
}

// Get the inputs to this command
set<tuple<shared_ptr<Artifact>, shared_ptr<Version>, InputType>> Command::getInputs()
    const noexcept {
  if (!_last_run) return {};

  return _last_run->inputs;
}

// Add an output to this command
void Command::addOutput(shared_ptr<Artifact> a, shared_ptr<Version> v) noexcept {
  _run->outputs.emplace(a, v);
}

// Get the outputs from this command
set<tuple<shared_ptr<Artifact>, shared_ptr<Version>>> Command::getOutputs() const noexcept {
  if (!_last_run) return {};

  return _last_run->outputs;
}
