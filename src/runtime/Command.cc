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

// Reset the transient state in this command to prepare for a new emulation/execution
void Command::reset() noexcept {
  // Clear the vector of references. They will be filled in again during emulation/execution.
  _refs.clear();
  _refs_use_count.clear();

  // Move the list of children to the last run list
  _last_run_children.clear();
  std::swap(_children, _last_run_children);
}

// Prepare this command to execute by creating dependencies and committing state
void Command::prepareToExecute(Build& build) noexcept {
  for (Command::RefID id = 0; id < _refs.size(); id++) {
    const auto& ref = _refs[id];

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

// Add an initial file descriptor to this command
void Command::addInitialFD(int fd, Command::RefID ref) noexcept {
  ASSERT(fd >= 0) << "Invalid file descriptor number " << fd << " in " << this;
  _initial_fds.emplace(fd, ref);
}

// Get a reference from this command's reference table
const shared_ptr<Ref>& Command::getRef(Command::RefID id) const noexcept {
  ASSERT(id >= 0 && id < _refs.size()) << "Invalid reference ID " << id << " in " << this;
  ASSERT(_refs[id]) << "Access to null reference ID " << id << " in " << this;
  return _refs[id];
}

// Store a reference at a known index of this command's local reference table
void Command::setRef(Command::RefID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= _refs.size()) _refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  ASSERT(!_refs[id]) << "Attempted to overwrite reference ID " << id << " in " << this;

  // Save the ref
  _refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Command::RefID Command::setRef(shared_ptr<Ref> ref) noexcept {
  RefID id = _refs.size();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;
  _refs.push_back(ref);
  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool Command::usingRef(Command::RefID id) noexcept {
  ASSERT(id >= 0 && id < _refs.size()) << "Invalid ref ID " << id << " in " << this;

  // Expand the use count vector if necessary
  if (_refs_use_count.size() <= id) _refs_use_count.resize(id + 1);

  // Increment the ref count. Is this the first use of the ref?
  if (_refs_use_count[id]++ == 0) {
    // This was the first use. Increment the user count in the ref, and return true
    _refs[id]->addUser();
    return true;
  }

  return false;
}

// Decrement this command's use counter for a Ref.
// Return true if that was the last use by this command.
bool Command::doneWithRef(Command::RefID id) noexcept {
  ASSERT(id >= 0 && id < _refs.size()) << "Invalid ref ID " << id << " in " << this;
  ASSERT(id < _refs_use_count.size() && _refs_use_count[id] > 0)
      << "Attempted to end an unknown use of ref r" << id << " in " << this;

  // Decrement the ref count. Was this the last use of the ref?
  if (--_refs_use_count[id] == 0) {
    // This was the last use. Decrement the user count in the ref and return true
    _refs[id]->removeUser();
    return true;
  }

  return false;
}

// Record that this command launched a child command
void Command::addChild(shared_ptr<Command> child) noexcept {
  _children.push_back(child);
}

// Look for a command that matches one of this command's children from the last run
shared_ptr<Command> Command::findChild(vector<string> args,
                                       Command::RefID exe_ref,
                                       Command::RefID cwd_ref,
                                       Command::RefID root_ref,
                                       map<int, Command::RefID> fds) noexcept {
  // Loop over this command's children from the last run
  for (auto iter = _last_run_children.begin(); iter != _last_run_children.end(); iter++) {
    const auto& child = *iter;

    // Does the child match the given launch parameters?
    // TODO: Check more than just arguments
    if (child->getArguments() == args) {
      // Removed the child from the list so it cannot be matched again
      _last_run_children.erase(iter);
      return child;
    }
  }

  // No match found
  return nullptr;
}
