#include "CommandRun.hh"

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

using std::list;
using std::map;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

// Get the command that produced this Run
shared_ptr<Command> CommandRun::getCommand() const noexcept {
  return _command;
}

// Prepare this command to execute by creating dependencies and committing state
void CommandRun::createLaunchDependencies(Build& build) noexcept {
  for (Ref::ID id = 0; id < _refs.size(); id++) {
    const auto& ref = _refs[id];

    // Is the ref assigned? If not, skip ahead
    if (!ref) continue;

    if (id == Ref::Cwd) {
      // The current directory has to exist to launch the command
      auto path = ref->getArtifact()->commitPath();
      if (path.has_value()) {
        LOG(exec) << "Committed cwd path " << path.value();
      } else {
        WARN << "Failed to commit path to current working directory " << ref->getArtifact();
      }

    } else {
      // All other referenced artifacts must be fully committed, except we'll ignore pipes for now
      if (ref->getArtifact()->as<PipeArtifact>()) continue;

      if (ref->getArtifact()->canCommitAll()) {
        ref->getArtifact()->commitAll();
      } else {
        WARN << "Launching " << _command << " without committing referenced artifact "
             << ref->getArtifact();
      }
    }
  }
}

// Get a reference from this command's reference table
const shared_ptr<Ref>& CommandRun::getRef(Ref::ID id) const noexcept {
  ASSERT(id >= 0 && id < _refs.size()) << "Invalid reference ID " << id << " in " << _command;
  ASSERT(_refs[id]) << "Access to null reference ID " << id << " in " << _command;
  return _refs[id];
}

// Store a reference at a known index of this command's local reference table
void CommandRun::setRef(Ref::ID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << _command;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= _refs.size()) _refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  // ASSERT(!_refs[id]) << "Attempted to overwrite reference ID " << id << " in " << _command;

  // Save the ref
  _refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Ref::ID CommandRun::setRef(shared_ptr<Ref> ref) noexcept {
  Ref::ID id = _refs.size();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << _command;
  _refs.push_back(ref);

  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool CommandRun::usingRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < _refs.size()) << "Invalid ref ID " << id << " in " << _command;

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
bool CommandRun::doneWithRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < _refs.size()) << "Invalid ref ID " << id << " in " << _command;
  ASSERT(id < _refs_use_count.size() && _refs_use_count[id] > 0)
      << "Attempted to end an unknown use of ref r" << id << " in " << _command;

  // Decrement the ref count. Was this the last use of the ref?
  if (--_refs_use_count[id] == 0) {
    // This was the last use. Decrement the user count in the ref and return true
    _refs[id]->removeUser();
    return true;
  }

  return false;
}

// Get this command's exit status
int CommandRun::getExitStatus() const noexcept {
  return _exit_status;
}

// Set this command's exit status, and record that it has exited
void CommandRun::setExitStatus(int status) noexcept {
  _exit_status = status;
}

// Record that this command launched a child command
void CommandRun::addChild(shared_ptr<CommandRun> child) noexcept {
  _children.push_back(child);
}

// Get this command's children
const list<shared_ptr<CommandRun>>& CommandRun::getChildren() const noexcept {
  return _children;
}

// Look for a command that matches one of this command's children from the last run
shared_ptr<Command> CommandRun::findChild(vector<string> args,
                                          Ref::ID exe_ref,
                                          Ref::ID cwd_ref,
                                          Ref::ID root_ref,
                                          map<int, Ref::ID> fds) noexcept {
  // Loop over this command's children from the last run
  for (auto& child : _children) {
    // Does the child match the given launch parameters?
    // TODO: Check more than just arguments
    if (!child->_matched) {
      auto child_cmd = child->getCommand();
      ASSERT(child_cmd) << "Run has no associated command";
      if (child->getCommand()->getArguments() == args) {
        // Mark the command as matched so we don't match it again
        child->_matched = true;
        return child->getCommand();
      }
    }
  }

  // No match found
  return nullptr;
}

// This command observed a change in a given scenario
void CommandRun::observeChange(Scenario s) noexcept {
  _changed.insert(s);
}

// An input to this command did not match the expected version
void CommandRun::inputChanged(shared_ptr<Artifact> artifact,
                              shared_ptr<MetadataVersion> observed,
                              shared_ptr<MetadataVersion> expected,
                              Scenario scenario) noexcept {
  _changed.insert(scenario);
}

// An input to this command did not match the expected version
void CommandRun::inputChanged(shared_ptr<Artifact> artifact,
                              shared_ptr<ContentVersion> observed,
                              shared_ptr<ContentVersion> expected,
                              Scenario scenario) noexcept {
  _changed.insert(scenario);
}

// Add an input to this command
void CommandRun::addMetadataInput(shared_ptr<Artifact> a, InputType t) noexcept {
  const auto& v = a->peekMetadata();
  _metadata_inputs.emplace(a, v, t);

  // If this command is running, make sure the metadata input is committed
  if (getCommand()->mustRerun()) a->commitMetadata();

  // If the version was created by another command, inform the creator that this command uses it
  if (auto creator = v->getCreator(); creator) {
    // If this is make accessing metadata, we only need to mark in one direction;
    // changing metadata alone does not need to trigger a re-execution of make
    if (getCommand()->isMake()) return;

    // Otherwise, add this command run to the creator's set of output users
    creator->currentRun()->_output_used_by.insert(shared_from_this());
  }
}

// Add an input to this command
void CommandRun::addContentInput(shared_ptr<Artifact> a,
                                 shared_ptr<ContentVersion> v,
                                 InputType t) noexcept {
  _content_inputs.emplace(a, v, t);

  // If this command is running, make sure the file is available
  // We can skip committing a version if this same command also created the version
  if (getCommand()->mustRerun() && !v->isCommitted() && v->getCreator() != getCommand()) {
    // Commit the version now
    ASSERT(a->canCommit(v)) << getCommand() << " accesses " << a << ", but version " << v
                            << " cannot be committed";

    a->commit(v);
  }

  // If the version was created by another command, inform the creator that this command uses it
  if (auto creator = v->getCreator(); creator) {
    // Otherwise, add this command run to the creator's set of output users
    creator->currentRun()->_output_used_by.insert(shared_from_this());
  }
}

// Add an output to this command
void CommandRun::addMetadataOutput(shared_ptr<Artifact> a, shared_ptr<MetadataVersion> v) noexcept {
  _metadata_outputs.emplace(a, v);
}

// Add an output to this command
void CommandRun::addContentOutput(shared_ptr<Artifact> a, shared_ptr<ContentVersion> v) noexcept {
  _content_outputs.emplace(a, v);
}

// An output from this command does not match the on-disk state (checked at the end of the build)
void CommandRun::outputChanged(shared_ptr<Artifact> artifact,
                               shared_ptr<ContentVersion> ondisk,
                               shared_ptr<ContentVersion> expected) noexcept {
  // If the expected output could be committed, there's no need to mark this command for rerun
  if (artifact->canCommit(expected)) return;

  LOGF(rebuild, "{} must rerun: on-disk state of {} has changed (expected {}, observed {})",
       getCommand(), artifact, expected, ondisk);

  _changed.insert(Scenario::Build);
  _changed.insert(Scenario::PostBuild);
}
