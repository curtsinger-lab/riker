#include "Run.hh"

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "runtime/Ref.hh"

// Get the command that produced this Run
shared_ptr<Command> Run::getCommand() const noexcept {
  return _command.lock();
}

// Prepare this command to execute by creating dependencies and committing state
void Run::createLaunchDependencies(Build& build) noexcept {
  for (Ref::ID id = 0; id < _refs.size(); id++) {
    const auto& ref = _refs[id];

    // Is the ref assigned? If not, skip ahead
    if (!ref) continue;

    if (id == Ref::Cwd) {
      // The current directory has to exist to launch the command
      ref->getArtifact()->mustExist(build, _command.lock());

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
const shared_ptr<Ref>& Run::getRef(Ref::ID id) const noexcept {
  ASSERT(id >= 0 && id < _refs.size())
      << "Invalid reference ID " << id << " in " << _command.lock();
  ASSERT(_refs[id]) << "Access to null reference ID " << id << " in " << _command.lock();
  return _refs[id];
}

// Store a reference at a known index of this command's local reference table
void Run::setRef(Ref::ID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= _refs.size()) _refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  // ASSERT(!_refs[id]) << "Attempted to overwrite reference ID " << id << " in " << this;

  // Save the ref
  _refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Ref::ID Run::setRef(shared_ptr<Ref> ref) noexcept {
  Ref::ID id = _refs.size();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;
  _refs.push_back(ref);

  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool Run::usingRef(Ref::ID id) noexcept {
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
bool Run::doneWithRef(Ref::ID id) noexcept {
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
int Run::getExitStatus() const noexcept {
  return _exit_status;
}

// Set this command's exit status, and record that it has exited
void Run::setExitStatus(int status) noexcept {
  _exit_status = status;
}

// Record that this command launched a child command
void Run::addChild(shared_ptr<Command> child) noexcept {
  _children.push_back(child);
}

// Get this command's children
const list<shared_ptr<Command>>& Run::getChildren() const noexcept {
  return _children;
}

// Look for a command that matches one of this command's children from the last run
shared_ptr<Command> Run::findChild(vector<string> args,
                                   Ref::ID exe_ref,
                                   Ref::ID cwd_ref,
                                   Ref::ID root_ref,
                                   map<int, Ref::ID> fds) noexcept {
  // Loop over this command's children from the last run
  for (auto iter = _children.begin(); iter != _children.end(); iter++) {
    const auto& child = *iter;

    // Does the child match the given launch parameters?
    // TODO: Check more than just arguments
    if (child->getArguments() == args) {
      // Removed the child from the list so it cannot be matched again
      _children.erase(iter);
      return child;
    }
  }

  // No match found
  return nullptr;
}

// This command observed a change in a given scenario
void Run::observeChange(Scenario s) noexcept {
  _changed.insert(s);
}

// Mark this command for re-execution
bool Run::markForRerun(RerunReason reason) noexcept {
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
bool Run::mustRerun() const noexcept {
  // Otherwise check the last run state
  return _rerun_reason.has_value();
}

// Add an input to this command
void Run::addInput(shared_ptr<Artifact> a, shared_ptr<Version> v, InputType t) noexcept {
  _inputs.emplace(a, v, t);
}

// Get the inputs to this command
set<tuple<shared_ptr<Artifact>, shared_ptr<Version>, InputType>> Run::getInputs() const noexcept {
  return _inputs;
}

// Add an output to this command
void Run::addOutput(shared_ptr<Artifact> a, shared_ptr<Version> v) noexcept {
  _outputs.emplace(a, v);
}

// Get the outputs from this command
set<tuple<shared_ptr<Artifact>, shared_ptr<Version>>> Run::getOutputs() const noexcept {
  return _outputs;
}
