#include "CommandRun.hh"

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "runtime/Ref.hh"
#include "versions/MetadataVersion.hh"

// Get the command that produced this Run
shared_ptr<Command> CommandRun::getCommand() const noexcept {
  return _command.lock();
}

// Prepare this command to execute by creating dependencies and committing state
void CommandRun::createLaunchDependencies(Build& build) noexcept {
  for (Ref::ID id = 0; id < _refs.size(); id++) {
    const auto& ref = _refs[id];

    // Is the ref assigned? If not, skip ahead
    if (!ref) continue;

    if (id == Ref::Cwd) {
      // The current directory has to exist to launch the command
      ref->getArtifact()->mustExist(_command.lock());

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
const shared_ptr<Ref>& CommandRun::getRef(Ref::ID id) const noexcept {
  ASSERT(id >= 0 && id < _refs.size())
      << "Invalid reference ID " << id << " in " << _command.lock();
  ASSERT(_refs[id]) << "Access to null reference ID " << id << " in " << _command.lock();
  return _refs[id];
}

// Store a reference at a known index of this command's local reference table
void CommandRun::setRef(Ref::ID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= _refs.size()) _refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  // ASSERT(!_refs[id]) << "Attempted to overwrite reference ID " << id << " in " << this;

  // Save the ref
  _refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Ref::ID CommandRun::setRef(shared_ptr<Ref> ref) noexcept {
  Ref::ID id = _refs.size();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;
  _refs.push_back(ref);

  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool CommandRun::usingRef(Ref::ID id) noexcept {
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
bool CommandRun::doneWithRef(Ref::ID id) noexcept {
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
    if (!child->_matched && child->getCommand()->getArguments() == args) {
      // Mark the command as matched so we don't match it again
      child->_matched = true;
      return child->getCommand();
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
                              shared_ptr<Version> observed,
                              shared_ptr<Version> expected,
                              Scenario scenario) noexcept {
  LOGF(rebuild, "{} changed in scenario {}: change in {} (expected {}, observed {})", getCommand(),
       scenario, artifact, expected, observed);
  _changed.insert(scenario);
}

// Mark this command for re-execution
bool CommandRun::markForRerun(RerunReason reason) noexcept {
  // Is this command already marked?
  bool already_marked = _rerun_reason.has_value();

  // If not, or if the given reason is "higher" than the previous marking, update it
  if (!already_marked || reason > _rerun_reason.value()) {
    _rerun_reason = reason;
  }

  // Return true if this was a new marking
  return !already_marked;
}

// Check to see if this command was marked for re-execution after the last run
bool CommandRun::mustRerun() const noexcept {
  // Otherwise check the last run state
  return _rerun_reason.has_value();
}

// Add an input to this command
void CommandRun::addInput(shared_ptr<Artifact> a, shared_ptr<Version> v, InputType t) noexcept {
  _inputs.emplace(a, v, t);

  // If this command is running, make sure the file is available
  // We can skip committing a version if this same command also created the version
  if (getCommand()->previousRun()->mustRerun() && !v->isCommitted() &&
      v->getCreator() != shared_from_this()) {
    // Commit the version now
    ASSERT(a->canCommit(v)) << getCommand() << " accesses " << a << ", but version " << v
                            << " cannot be committed";

    a->commit(v);
  }

  // If the version was created by another command, inform the creator that this command uses it
  if (auto creator = v->getCreator(); creator) {
    // If this is make accessing metadata, we only need to mark in one direction;
    // changing metadata alone does not need to trigger a re-execution of make
    if (v->as<MetadataVersion>() && getCommand()->isMake()) return;

    // If the only requirement is that the artifact exists, we don't need to create a dependency
    if (t == InputType::Exists) return;

    // Otherwise, add this command run to the creator's set of output users
    creator->_output_used_by.insert(shared_from_this());
  }
}

// Add an output to this command
void CommandRun::addOutput(shared_ptr<Artifact> a, shared_ptr<Version> v) noexcept {
  _outputs.emplace(a, v);
}
