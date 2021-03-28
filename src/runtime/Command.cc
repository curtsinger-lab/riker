#include "Command.hh"

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "tracing/Process.hh"
#include "ui/options.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"

using std::list;
using std::make_shared;
using std::make_unique;
using std::map;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;

namespace fs = std::filesystem;

/// Keep track of how many times a possible command line argument has been included
map<string, size_t> argument_counts;

/// Keep track of the total number of commands with arguments
size_t command_count = 0;

/// Get a shared pointer to the special null command instance
const shared_ptr<Command>& Command::getNullCommand() noexcept {
  static shared_ptr<Command> _null_command(new Command());
  return _null_command;
}

// Create a command
Command::Command(vector<string> args) noexcept : _args(args) {
  ASSERT(args.size() > 0) << "Attempted to create a command with no arguments";

  command_count++;

  // Add each argument (other than the first) to the argument_counts map
  for (size_t i = 1; i < args.size(); i++) {
    argument_counts[args[i]]++;
  }
}

// Destroy a command. The destructor has to be declared in the .cc file where we have a complete
// definition of Run
Command::~Command() noexcept = default;

// Get a short, length-limited name for this command
string Command::getShortName(size_t limit) const noexcept {
  // A command with no arguments is anonymous. This shouldn't happen, but better to be safe.
  if (_args.size() == 0) return "<anon>";

  if (_short_name_command_count < command_count) _short_names.clear();

  // Do we need to create a new abbreviated name for this command?
  if (!_short_names[limit].has_value()) {
    // The short command name always starts with the executable filename
    // After that, we include arguments up to the character limit. Any omitted arguments are
    // replaced by ellipsis, and the remaining arguments are selected to (approximately) maximize
    // the "information density" in the abbreviation. The more common an argument is, the less
    // information it conveys. The density is just that information divided by the space taken up by
    // the argument. There are a few heuristics to avoid creating short "orphaned" arguments with
    // ellipsis on both sides.

    // The first argument to the command is its name. Treat it as a path for now
    fs::path exe_path = _args.front();
    if (exe_path.is_absolute()) exe_path = exe_path.filename();

    // The output starts with the executable name
    string result = exe_path;

    // Which arguments will we include? Initially, all of them except the first.
    vector<bool> include_arg(_args.size(), true);
    include_arg[0] = false;

    // We'll also keep track of the value of each argument
    vector<float> arg_values(_args.size());
    for (size_t i = 1; i < _args.size(); i++) {
      // How often does this argument appear?
      size_t frequency = argument_counts[_args[i]];
      if (frequency < 1) frequency = 1;

      // What is the information value of the argument?
      arg_values[i] = 1.0 / frequency;

      // Is this a possible filename preceded by a two-character flag? (e.g. -c somefile.c)
      if (_args[i][0] != '-' && _args[i - 1][0] == '-' && _args[i - 1].size() == 2) {
        // Increase the value of both arguments so we (hopefully) retain them
        arg_values[i - 1] += 1;
        arg_values[i] += 1;
      }
    }

    // And the space occupied by including each argument
    vector<int> arg_space(_args.size());
    for (size_t i = 1; i < _args.size(); i++) {
      // Initially each argument occupies space proportional to its length, minus the space required
      // to print ellipsis in its place
      arg_space[i] = _args[i].size() - 2;
    }

    // Loop until the output is short enough to print
    size_t length = result.size();
    for (size_t i = 1; i < _args.size(); i++) {
      length += _args[i].size() + 1;
    }

    // Remove arguments until the final length is below the limit
    while (length > limit) {
      // Find the least information-dense included argument
      float min_density;
      size_t remove_index = 0;
      for (size_t i = 1; i < _args.size(); i++) {
        // Skip arguments that are already excluded
        if (!include_arg[i]) continue;

        // Compute the information density of argument i
        float density = arg_values[i] / arg_space[i];

        // If the density is negative, deleting the argument will cost space. Set it arbitrarily
        // high
        if (density <= 0) density = 1.0;

        // Is this a new minimum?
        if (remove_index == 0 || density < min_density) {
          min_density = density;
          remove_index = i;
        }
      }

      // If there was no minimum, we're done
      if (remove_index == 0) break;

      // Exclude the lowest-density argument
      include_arg[remove_index] = false;

      // If there is a preceding argument, its space requirement increases by the size of the
      // ellipsis
      if (remove_index > 1) {
        arg_space[remove_index - 1] += 2;
      }

      // If there is a following argument, its space requirement increases by the size of the
      // ellipsis
      if (remove_index < _args.size() - 1) {
        arg_space[remove_index + 1] += 2;
      }

      // If the argument before the removed one is short and stuck between two removed args, drop it
      if (remove_index > 2 && !include_arg[remove_index - 2]) {
        if (_args[remove_index - 1].size() <= 6) include_arg[remove_index - 1] = false;
      }

      if (remove_index < _args.size() - 2 && !include_arg[remove_index + 2]) {
        if (_args[remove_index + 1].size() <= 6) include_arg[remove_index + 1] = false;
      }

      // Compute the new output length
      length = result.size();
      bool ellipsis = false;
      for (int i = 1; i < _args.size(); i++) {
        if (include_arg[i]) {
          length += _args[i].size() + 1;
          ellipsis = false;
        } else if (!ellipsis) {
          length += 2;
          ellipsis = true;
        }
      }
    }

    // Build the output
    bool ellipsis = false;
    for (size_t i = 1; i < _args.size(); i++) {
      if (include_arg[i]) {
        result += " " + _args[i];
        ellipsis = false;
      } else if (!ellipsis) {
        result += " …";
        ellipsis = true;
      }
    }

    if (length > limit) {
      result = result.substr(0, limit - 1) + "…";
    }

    _short_names[limit] = result;
    _short_name_command_count = command_count;
  }

  return _short_names[limit].value();
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

// Get the command run data for the current run
const unique_ptr<Command::Run>& Command::currentRun() noexcept {
  if (!_run) _run = make_unique<Command::Run>();
  return _run;
}

// Get the command run data for the previous run
const unique_ptr<Command::Run>& Command::previousRun() noexcept {
  if (!_last_run) _last_run = make_unique<Command::Run>();
  return _last_run;
}

// Finish the current run and set up for another one
void Command::finishRun() noexcept {
  // Create a new instance of CommandRun in _last_run, then swap them
  _last_run = make_unique<Command::Run>();
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
  if (previousRun()->_changed.size() == 2) {
    if (mark(RebuildMarking::MustRun)) {
      LOGF(rebuild, "{} must run: input changed or output is missing/modified", this);
    }
  }
}

// Assign a marking to this command. Return true if the marking is new.
bool Command::mark(RebuildMarking m) noexcept {
  // See rebuild planning rules in docs/new-rebuild.md

  // If lazy builds are turned off, promote a MayRun marking to MustRun
  if (!options::lazy_builds && m == RebuildMarking::MayRun) m = RebuildMarking::MustRun;

  // Check the new marking
  if (m == RebuildMarking::MustRun) {
    // If this command already had an equivalent or higher marking, return false.
    // There's no need to propagate this marking because it is not new.
    if (_marking == RebuildMarking::MustRun || _marking == RebuildMarking::AlreadyRun) {
      return false;
    }

    // Update the marking
    _marking = RebuildMarking::MustRun;

    // Rule 3: For each command D that produces uncached input V to C: mark D as MustRun
    for (const auto& [weak_producer, info] : previousRun()->_needs_output_from) {
      auto producer = weak_producer.lock();
      auto [a, v] = info;
      if (producer->mark(RebuildMarking::MustRun)) {
        LOGF(rebuild, "{} must run: version {} of {} is needed by {}", producer, v, a, this);
      }
    }

    // Rule 4: For each command D that produces input V to C: if D is marked MayRun, mark D as
    // MustRun
    /*for (const auto& [weak_producer, info] : previousRun()->_uses_output_from) {
      auto producer = weak_producer.lock();
      auto [a, v] = info;
      if (producer->_marking == RebuildMarking::MayRun) {
        producer->mark(RebuildMarking::MustRun);
        LOGF(
            rebuild,
            "{} must run: may change output {} of {} needed by {}, which is already marked for run",
            producer, v, a, this);
      }
    }*/

    // Rule 5: For each command D that consumes output V from C: if V is cached mark D as MayRun. If
    // not, mark D as MustRun.

    // Mark the MustRun commands first to avoid marking them a second time
    for (const auto& [weak_user, info] : previousRun()->_output_needed_by) {
      auto user = weak_user.lock();
      auto [a, v] = info;
      if (user->mark(RebuildMarking::MustRun)) {
        LOGF(rebuild, "{} must run: uncached input {} of {} may be changed by {}", user, v, a,
             this);
      }
    }

    // Now do the MayRun markings
    for (const auto& [weak_user, info] : previousRun()->_output_used_by) {
      auto user = weak_user.lock();
      auto [a, v] = info;
      if (user->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: input {} of {} may be changed by {}", user, v, a, this);
      }
    }

    // Rule a: Mark all of this command's children as MustRun
    /*for (const auto& child : previousRun()->_children) {
      if (child->mark(RebuildMarking::MustRun)) {
        LOGF(rebuild, "{} must run: parent {} is running", child, this);
      }
    }*/

    // Rule b: If this command's parent is marked MayRun, change it to MustRun
    /*auto parent = previousRun()->_parent.lock();
    if (parent && parent->_marking == RebuildMarking::MayRun) {
      parent->mark(RebuildMarking::MustRun);
      LOGF(rebuild, "{} must run: child {} is running and cannot be skipped", parent, this);
    }*/

    // The marking was new, so return true
    return true;

  } else if (m == RebuildMarking::MayRun) {
    // If this command already had an equivalent or higher marking, return false.
    // There's no need to propagate this marking becasue it is not new.
    if (_marking == RebuildMarking::MayRun || _marking == RebuildMarking::MustRun ||
        _marking == RebuildMarking::AlreadyRun) {
      return false;
    }

    // Update the marking
    _marking = RebuildMarking::MayRun;

    // Rule 6: For each command D that produces uncached input V to C: mark D as MayRun.
    for (const auto& [weak_producer, info] : previousRun()->_needs_output_from) {
      auto producer = weak_producer.lock();
      auto [a, v] = info;
      if (producer->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: version {} of {} may be needed by {}", producer, v, a, this);
      }
    }

    // Rule 7: For each command D that consumes output V from C: mark D as MayRun
    for (const auto& [weak_user, info] : previousRun()->_output_used_by) {
      auto user = weak_user.lock();
      auto [a, v] = info;
      if (user->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: input {} of {} may be changed by {}", user, v, a, this);
      }
    }

    // Rule 8: For each command D that consumes output V from C: if D is marked MustRun, mark C as
    // MustRun
    /*for (const auto& [weak_user, info] : previousRun()->_output_used_by) {
      auto user = weak_user.lock();
      auto [a, v] = info;
      if (user->_marking == RebuildMarking::MustRun) {
        mark(RebuildMarking::MustRun);
        LOGF(rebuild, "{} must run: output {} of {} is needed by command {}", this, v, a, user);
      }
    }*/

    // Rule c: Mark all of this command's children as MayRun
    /*for (const auto& child : previousRun()->_children) {
      if (child->mark(RebuildMarking::MayRun)) {
        LOGF(rebuild, "{} may run: parent {} may run", child, this);
      }
    }*/

    // The marking was new, so return true
    return true;

  } else {
    // Emulate and AlreadyRun are never new markings
    return false;
  }
}

/******************** Current Run Data ********************/

// Prepare this command to execute by creating dependencies and committing state
void Command::createLaunchDependencies() noexcept {
  for (Ref::ID id = 0; id < currentRun()->_refs.size(); id++) {
    const auto& ref = currentRun()->_refs[id];

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
      // Access the content of the artifact to force any required commits
      ref->getArtifact()->getContent(shared_from_this());
    }
  }
}

// Record that this command launched a child command
void Command::addChild(shared_ptr<Command> child) noexcept {
  currentRun()->_children.push_back(child);
  child->currentRun()->_parent = shared_from_this();
}

// Check if the latest run of this command has been launched yet
bool Command::isLaunched() noexcept {
  // The null command is launched by default
  return isNullCommand() || currentRun()->_launched;
}

// Mark the latest run of this command as launched
void Command::setLaunched(shared_ptr<Process> p) noexcept {
  currentRun()->_launched = true;
  currentRun()->_process = p;
}

const shared_ptr<Process>& Command::getProcess() noexcept {
  return currentRun()->_process;
}

// Get this command's exit status for the current run
int Command::getExitStatus() noexcept {
  return currentRun()->_exit_status;
}

// Set this command's exit status, and record that it has exited
void Command::setExitStatus(int status) noexcept {
  currentRun()->_exit_status = status;
}

// Get a reference from this command's reference table
const shared_ptr<Ref>& Command::getRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < currentRun()->_refs.size())
      << "Invalid reference ID " << id << " in " << this;
  ASSERT(currentRun()->_refs[id]) << "Access to null reference ID " << id << " in " << this;
  return currentRun()->_refs[id];
}

// Store a reference at a known index of this command's local reference table
void Command::setRef(Ref::ID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= currentRun()->_refs.size()) currentRun()->_refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  // ASSERT(!currentRun()->_refs[id]) << "Attempted to overwrite reference ID " << id << " in " <<
  // this

  // Save the ref
  currentRun()->_refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Ref::ID Command::setRef(shared_ptr<Ref> ref) noexcept {
  Ref::ID id = currentRun()->_refs.size();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;
  currentRun()->_refs.push_back(ref);

  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool Command::usingRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < currentRun()->_refs.size()) << "Invalid ref ID " << id << " in " << this;

  // Expand the use count vector if necessary
  if (currentRun()->_refs_use_count.size() <= id) currentRun()->_refs_use_count.resize(id + 1);

  // Increment the ref count. Is this the first use of the ref?
  if (currentRun()->_refs_use_count[id]++ == 0) {
    // This was the first use. Increment the user count in the ref, and return true
    currentRun()->_refs[id]->addUser();
    return true;
  }

  return false;
}

// Decrement this command's use counter for a Ref.
// Return true if that was the last use by this command.
bool Command::doneWithRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < currentRun()->_refs.size()) << "Invalid ref ID " << id << " in " << this;
  ASSERT(id < currentRun()->_refs_use_count.size() && currentRun()->_refs_use_count[id] > 0)
      << "Attempted to end an unknown use of ref r" << id << " in " << this;

  // Decrement the ref count. Was this the last use of the ref?
  if (--currentRun()->_refs_use_count[id] == 0) {
    // This was the last use. Decrement the user count in the ref and return true
    currentRun()->_refs[id]->removeUser();
    return true;
  }

  return false;
}

// This command observed a change in a given scenario
void Command::observeChange(Scenario s) noexcept {
  currentRun()->_changed.insert(s);
}

// An input to this command did not match the expected version
void Command::inputChanged(shared_ptr<Artifact> artifact,
                           shared_ptr<MetadataVersion> observed,
                           shared_ptr<MetadataVersion> expected,
                           Scenario scenario) noexcept {
  currentRun()->_changed.insert(scenario);
}

// An input to this command did not match the expected version
void Command::inputChanged(shared_ptr<Artifact> artifact,
                           shared_ptr<ContentVersion> observed,
                           shared_ptr<ContentVersion> expected,
                           Scenario scenario) noexcept {
  currentRun()->_changed.insert(scenario);
}

// Add an input to this command
void Command::addMetadataInput(shared_ptr<Artifact> a,
                               shared_ptr<MetadataVersion> v,
                               shared_ptr<Command> writer) noexcept {
  if (options::track_inputs_outputs) currentRun()->_inputs.emplace_back(a, v, writer);

  // If this command wrote the version there's no need to do any additional tracking
  if (writer.get() == this) return;

  // If this command is running, make sure the metadata is committed
  if (mustRun()) a->commitMetadata();

  // If the version was created by another command, track the use of that command's output
  if (writer) {
    // This command uses output from writer
    currentRun()->_uses_output_from.emplace(writer, std::tuple{a, v});

    // Otherwise, add this command run to the creator's set of output users
    writer->currentRun()->_output_used_by.emplace(shared_from_this(), std::tuple{a, v});
  }
}

// Add an input to this command
void Command::addContentInput(shared_ptr<Artifact> a,
                              shared_ptr<ContentVersion> v,
                              shared_ptr<Command> writer) noexcept {
  if (options::track_inputs_outputs) currentRun()->_inputs.emplace_back(a, v, writer);

  // If this command wrote the version there's no need to do any additional tracking
  if (writer.get() == this) return;

  // If this command wrote the version there's no need to do any additional tracking
  if (writer.get() == this) return;

  // If this command is running, make sure the file is available
  if (mustRun()) a->commitContent();

  // If the version was created by another command, track the use of that command's output
  if (writer) {
    // This command uses output from writer
    currentRun()->_uses_output_from.emplace(writer, std::tuple{a, v});
    writer->currentRun()->_output_used_by.emplace(shared_from_this(), std::tuple{a, v});

    // Is the version committable? If not, this command NEEDS output from writer
    if (!v->canCommit()) {
      currentRun()->_needs_output_from.emplace(writer, std::tuple{a, v});
      writer->currentRun()->_output_needed_by.emplace(shared_from_this(), std::tuple{a, v});
    }
  }
}

// Add an input to this command
void Command::addDirectoryInput(std::shared_ptr<Artifact> a,
                                std::shared_ptr<DirVersion> v,
                                std::shared_ptr<Command> writer) noexcept {
  if (!v) return;

  if (options::track_inputs_outputs) currentRun()->_inputs.emplace_back(a, v, writer);

  // If this command is running, make sure the directory version is committed
  if (mustRun()) {
    // We'll need to treat the artifact as a DirArtifact
    auto dir = a->as<DirArtifact>();
    ASSERT(dir) << "Non-directory artifact " << a << " passed to addDirectoryInput";

    // Does the directory version refer to a specific entry?
    auto e = v->getEntry();
    if (e.has_value()) {
      // Yes. Commit that entry
      dir->commitEntry(e.value());
    } else {
      // No. Just commit the base content
      a->commitContent();
    }
  }

  // If the version was created by another command, track the use of that command's output
  if (writer) {
    // This command uses output from writer
    currentRun()->_uses_output_from.emplace(writer, std::tuple{a, v});
    writer->currentRun()->_output_used_by.emplace(shared_from_this(), std::tuple{a, v});
  }
}

// Add an output to this command
void Command::addMetadataOutput(shared_ptr<Artifact> a, shared_ptr<MetadataVersion> v) noexcept {
  if (options::track_inputs_outputs) currentRun()->_outputs.emplace_back(a, v);
}

// Add an output to this command
void Command::addContentOutput(shared_ptr<Artifact> a, shared_ptr<ContentVersion> v) noexcept {
  if (options::track_inputs_outputs) currentRun()->_outputs.emplace_back(a, v);
}

// Add an output to this command
void Command::addDirectoryOutput(shared_ptr<Artifact> a, shared_ptr<DirVersion> v) noexcept {
  if (options::track_inputs_outputs) currentRun()->_outputs.emplace_back(a, v);
}

// An output from this command does not match the on-disk state (checked at the end of the build)
void Command::outputChanged(shared_ptr<Artifact> artifact,
                            shared_ptr<ContentVersion> ondisk,
                            shared_ptr<ContentVersion> expected) noexcept {
  // If the expected output could be committed, there's no need to mark this command for rerun
  if (expected->canCommit()) return;

  LOGF(rebuild, "{} must rerun: on-disk state of {} has changed (expected {}, observed {})", this,
       artifact, expected, ondisk);

  currentRun()->_changed.insert(Scenario::Build);
  currentRun()->_changed.insert(Scenario::PostBuild);
}

/********************** Previous Run Data ********************/

/// Get this command's list of children
const std::list<std::shared_ptr<Command>>& Command::getChildren() noexcept {
  return previousRun()->_children;
}

// Look for a command that matches one of this command's children from the last run
shared_ptr<Command> Command::findChild(vector<string> args,
                                       Ref::ID exe_ref,
                                       Ref::ID cwd_ref,
                                       Ref::ID root_ref,
                                       map<int, Ref::ID> fds) noexcept {
  // Loop over this command's children from the last run
  for (auto& child : previousRun()->_children) {
    // Does the child match the given launch parameters?
    // TODO: Check more than just arguments
    if (!child->previousRun()->_matched) {
      if (child->getArguments() == args) {
        // Mark the command as matched so we don't match it again
        child->previousRun()->_matched = true;
        return child;
      }
    }
  }

  // No match found
  return nullptr;
}

/// Get the content inputs to this command
const Command::InputList& Command::getInputs() noexcept {
  return previousRun()->_inputs;
}

/// Get the content outputs from this command
const Command::OutputList& Command::getOutputs() noexcept {
  return previousRun()->_outputs;
}
