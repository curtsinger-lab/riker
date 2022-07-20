#include "Command.hh"

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "runtime/env.hh"
#include "tracing/Process.hh"
#include "util/options.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"

using std::list;
using std::make_shared;
using std::make_unique;
using std::map;
using std::nullopt;
using std::optional;
using std::set;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;

namespace fs = std::filesystem;

/// Keep track of how many times a possible command line argument has been included
map<string, size_t> argument_counts;

/// Keep track of the total number of commands with arguments
size_t command_count = 0;

// Create a command
Command::Command(vector<string> args) noexcept : _args(args) {
  // If this is a null command with no arguments, mark it as executed
  if (args.size() == 0) _executed = true;

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
  if (_args.size() == 0) return "rkr";

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
      arg_values[i] = 1.0 / (frequency * frequency);

      // Is this a possible filename preceded by a two-character flag? (e.g. -c somefile.c)
      if (_args[i][0] != '-' && _args[i - 1] == "-c") {
        // Increase the value of both arguments so we (hopefully) retain them
        arg_values[i - 1] += 4;
        arg_values[i] += 4;
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
  for (string arg : _args) {
    if (!first) result += " ";
    first = false;

    // Escape newlines in the argument if there are any
    size_t pos = 0;
    while ((pos = arg.find('\n', pos)) != string::npos) {
      arg.replace(pos, 1, "\\n");
    }

    result += arg;
  }
  return result;
}

// Is this command empty?
bool Command::isEmptyCommand() const noexcept {
  return _args.size() == 0;
}

// Finish the current run and set up for another one
void Command::finishRun() noexcept {
  // The current run becomes the previous run
  _previous_run = std::move(_current_run);
  _current_run = Command::Run();

  // At the end of a build phase, all commands return to a non-running plan
  _must_run = false;

  // Recursively finish the run for all children
  for (const auto& child : _previous_run._children) {
    child->finishRun();
  }
}

// Plan the next build based on this command's completed run
void Command::planBuild() noexcept {
  // See rebuild planning rules in docs/new-rebuild.md
  // Rules 1 & 2: If this command observe a change on its previous run, mark it for rerun
  if (_previous_run._changed == Scenario::Both) {
    if (mark()) {
      LOGF(rebuild, "{} must run: input changed or output is missing/modified", this);
    }
  }

  // Recursively call planBuild on all children
  for (const auto& child : _previous_run._children) {
    child->planBuild();
  }
}

// Does this command or any of its descendants need to run? If not, return true.
bool Command::allFinished() const noexcept {
  if (mustRun()) {
    LOG(rebuild) << this << " must run";
    return false;
  }

  for (const auto& child : _previous_run._children) {
    if (!child->allFinished()) return false;
  }

  return true;
}

// Get a set of all commands including this one and its descendants
set<shared_ptr<Command>> Command::collectCommands() noexcept {
  set<shared_ptr<Command>> result;
  result.insert(shared_from_this());
  for (const auto& child : getChildren()) {
    for (const auto& include : child->collectCommands()) {
      result.insert(include);
    }
  }

  return result;
}

// Get a set of all commands that must run from this command and its descendants
set<shared_ptr<Command>> Command::collectMustRun() noexcept {
  set<shared_ptr<Command>> result;
  if (mustRun()) {
    result.insert(shared_from_this());
  }

  for (const auto& child : getChildren()) {
    for (const auto& include : child->collectMustRun()) {
      result.insert(include);
    }
  }

  return result;
}

// Mark this command for execution. Return true if the marking is new.
bool Command::mark() noexcept {
  // If this command already has to run, the marking is not new
  if (_must_run) return false;

  // Mark the command to run
  _must_run = true;

  // For each command D that produces uncached input V to C: D must run.
  //   (C must run, so produce all of its uncached inputs)
  for (const auto& weak_producer : _previous_run._needs_output_from) {
    auto producer = weak_producer.lock();
    if (producer->mark()) {
      LOGF(rebuild, "{} must run: {} requires output for its run", producer, this);
    }
  }

  // For each command D that consumes uncached output V from C: D must run.
  //   (C must run, so also run anything that consumes its uncached outputs)
  for (const auto& weak_user : _previous_run._output_needed_by) {
    auto user = weak_user.lock();
    if (user->mark()) {
      LOGF(rebuild, "{} must run: {} may change uncached input during its run", user, this);
    }
  }

  // The marking was new, so return true
  return true;
}

/******************** Current Run Data ********************/

// Prepare this command to execute by creating dependencies and committing state
void Command::createLaunchDependencies() noexcept {
  for (Ref::ID id = 0; id < _current_run._refs.size(); id++) {
    const auto& ref = _current_run._refs[id];

    // Is the ref assigned? If not, skip ahead
    if (!ref) continue;

    if (id == Ref::Cwd) {
      // The current directory has to exist to launch the command
      auto path = ref->getArtifact()->commitPath();
      if (!path.has_value()) {
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
  _current_run._children.push_back(child);
  child->_current_run._parent = shared_from_this();
}

// Check if the latest run of this command has been launched yet
bool Command::isLaunched() noexcept {
  // The empty command is launched by default
  return _current_run._launched;
}

// Mark the latest run of this command as launched
void Command::setLaunched(shared_ptr<Process> p) noexcept {
  _current_run._launched = true;
  _current_run._process = p;
}

const shared_ptr<Process>& Command::getProcess() noexcept {
  return _current_run._process;
}

// Get this command's exit status for the current run
int Command::getExitStatus() noexcept {
  return _current_run._exit_status;
}

// Set this command's exit status, and record that it has exited
void Command::setExitStatus(int status) noexcept {
  _current_run._exit_status = status;
}

// Apply a set of substitutions to this command and save the mappings for future paths
void Command::applySubstitutions(map<string, string> substitutions) noexcept {
  _short_names.clear();
  for (size_t i = 0; i < _args.size(); i++) {
    auto iter = substitutions.find(_args[i]);
    if (iter != substitutions.end()) {
      _args[i] = iter->second;
    }
  }

  // Save the path substitution map
  _current_run._substitutions = std::move(substitutions);
}

// Look for a matching path substitution and return the path this command should use
string Command::substitutePath(string p) noexcept {
  auto iter = _current_run._substitutions.find(p);
  if (iter == _current_run._substitutions.end()) return p;

  LOG(exec) << this << ": Replacing path " << p << " with " << iter->second;

  return iter->second;
}

// Inform this command that it used a temporary file
void Command::addTempfile(shared_ptr<Artifact> tempfile) noexcept {
  // Add the tempfile and mark it as not accessed (the value in the map)
  _current_run._tempfiles.emplace(tempfile, false);
}

// Get a reference from this command's reference table
const shared_ptr<Ref>& Command::getRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < _current_run._refs.size())
      << "Invalid reference ID " << id << " in " << this;
  ASSERT(_current_run._refs[id]) << "Access to null reference ID " << id << " in " << this;
  return _current_run._refs[id];
}

// Store a reference at a known index of this command's local reference table
void Command::setRef(Ref::ID id, shared_ptr<Ref> ref) noexcept {
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;

  // Are we adding this ref onto the end of the refs list? If so, grow as needed
  if (id >= _current_run._refs.size()) _current_run._refs.resize(id + 1);

  // Make sure the ref we're assigning to is null
  ASSERT(!_current_run._refs[id]) << "Attempted to overwrite reference ID " << id << " in " << this;

  // Save the ref
  _current_run._refs[id] = ref;
}

// Store a reference at the next available index of this command's local reference table
Ref::ID Command::setRef(shared_ptr<Ref> ref) noexcept {
  Ref::ID id = nextRef();
  ASSERT(ref) << "Attempted to store null ref at ID " << id << " in " << this;
  _current_run._refs[id] = ref;

  return id;
}

// Get the next available Ref ID
Ref::ID Command::nextRef() noexcept {
  Ref::ID id = _current_run._refs.size();

  // Make sure the newly claimed reference is not in the reserved range
  if (id < Ref::ReservedRefs) id = Ref::ReservedRefs;

  // Make space for the newly reserved reference ID in the refs array
  if (id >= _current_run._refs.size()) _current_run._refs.resize(id + 1);

  return id;
}

// Increment this command's use counter for a Ref.
// Return true if this is the first use by this command.
bool Command::usingRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < _current_run._refs.size()) << "Invalid ref ID " << id << " in " << this;

  // Expand the use count vector if necessary
  if (_current_run._refs_use_count.size() <= id) _current_run._refs_use_count.resize(id + 1);

  // Increment the ref count. Is this the first use of the ref?
  if (_current_run._refs_use_count[id]++ == 0) {
    // This was the first use. Increment the user count in the ref, and return true
    _current_run._refs[id]->addUser();
    return true;
  }

  return false;
}

// Decrement this command's use counter for a Ref.
// Return true if that was the last use by this command.
bool Command::doneWithRef(Ref::ID id) noexcept {
  ASSERT(id >= 0 && id < _current_run._refs.size()) << "Invalid ref ID " << id << " in " << this;
  ASSERT(id < _current_run._refs_use_count.size() && _current_run._refs_use_count[id] > 0)
      << "Attempted to end an unknown use of ref r" << id << " in " << this;

  // Decrement the ref count. Was this the last use of the ref?
  if (--_current_run._refs_use_count[id] == 0) {
    // This was the last use. Decrement the user count in the ref and return true
    _current_run._refs[id]->removeUser();
    return true;
  }

  return false;
}

// This command observed a change in a given scenario
void Command::observeChange(Scenario s) noexcept {
  _current_run._changed |= s;
}

// An input to this command did not match the expected version
void Command::inputChanged(shared_ptr<Artifact> artifact,
                           MetadataVersion observed,
                           MetadataVersion expected,
                           Scenario scenario) noexcept {
  _current_run._changed |= scenario;
}

// An input to this command did not match the expected version
void Command::inputChanged(shared_ptr<Artifact> artifact,
                           shared_ptr<ContentVersion> observed,
                           shared_ptr<ContentVersion> expected,
                           Scenario scenario) noexcept {
  _current_run._changed |= scenario;
}

// Add an input to this command
void Command::addMetadataInput(shared_ptr<Artifact> a,
                               shared_ptr<MetadataVersion> v,
                               shared_ptr<Command> writer) noexcept {
  if (options::track_inputs_outputs) _current_run._inputs.emplace_back(a, v, writer);

  // If this command wrote the version there's no need to do any additional tracking
  if (writer.get() == this) return;

  // If this command is running, make sure the metadata is committed
  if (mustRun()) a->commitMetadata();
}

// Add an input to this command
void Command::addContentInput(shared_ptr<Artifact> a,
                              shared_ptr<ContentVersion> v,
                              shared_ptr<Command> writer) noexcept {
  if (options::track_inputs_outputs) _current_run._inputs.emplace_back(a, v, writer);

  // Is the artifact one of our temporary files?
  if (auto iter = _current_run._tempfiles.find(a); iter != _current_run._tempfiles.end()) {
    // Is this the first access to the temporary file?
    if (iter->second == false) {
      // Mark the tempfile as accessed now
      iter->second = true;

      // Is this an access from a different command, or no command at all?
      if (writer.get() != this) {
        // Get a path to the artifact
        auto path = a->getPath();

        // Do we have a usable path?
        if (path.has_value()) {
          // Record the dependency on the temporary file content
          _current_run._tempfile_expected_content.emplace(path.value().string(), v);
        }
      }
    }
  }

  // If this command wrote the version there's no need to do any additional tracking
  if (writer.get() == this) return;

  // If this command is running, make sure the file is available
  if (mustRun()) a->commitContent();

  // If the version was created by another command, track the use of that command's output
  if (writer) {
    // Is the version committable?
    if (!v->canCommit()) {
      // No. Is the input uncommitted? If so, the writer must produce it for this command
      if (a->hasUncommittedContent()) {
        _current_run._needs_output_from.emplace(writer);
      }

      // If the writer has to run, the reader must also run.
      writer->_current_run._output_needed_by.emplace(shared_from_this());
    }
  }
}

// Add an input to this command
void Command::addDirectoryInput(std::shared_ptr<Artifact> a,
                                std::shared_ptr<DirVersion> v,
                                std::shared_ptr<Command> writer) noexcept {
  if (!v) return;

  if (options::track_inputs_outputs) _current_run._inputs.emplace_back(a, v, writer);

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
}

// Add an output to this command
void Command::addMetadataOutput(shared_ptr<Artifact> a, shared_ptr<MetadataVersion> v) noexcept {
  if (options::track_inputs_outputs) _current_run._outputs.emplace_back(a, v);
}

// Add an output to this command
void Command::addContentOutput(shared_ptr<Artifact> a, shared_ptr<ContentVersion> v) noexcept {
  if (options::track_inputs_outputs) _current_run._outputs.emplace_back(a, v);
}

// Add an output to this command
void Command::addDirectoryOutput(shared_ptr<Artifact> a, shared_ptr<DirVersion> v) noexcept {
  if (options::track_inputs_outputs) _current_run._outputs.emplace_back(a, v);
}

// An output from this command does not match the on-disk state (checked at the end of the build)
void Command::outputChanged(shared_ptr<Artifact> artifact,
                            shared_ptr<ContentVersion> ondisk,
                            shared_ptr<ContentVersion> expected) noexcept {
  // If the expected output could be committed, there's no need to mark this command for rerun
  if (expected->canCommit()) return;

  LOGF(rebuild, "{} must rerun: on-disk state of {} has changed (expected {}, observed {})", this,
       artifact, expected, ondisk);

  _current_run._changed |= Scenario::Both;
}

/********************** Previous Run Data ********************/

/// Get this command's list of children
const list<shared_ptr<Command>>& Command::getChildren() noexcept {
  return _previous_run._children;
}

optional<map<string, string>> Command::tryToMatch(const vector<string>& other_args,
                                                  const map<int, Ref::ID>& fds) const noexcept {
  // If the argument arrays are different lengths, there cannot be a match
  if (other_args.size() != _args.size()) return nullopt;

  // Do the initial FDs cover all the FDs this command needs?
  for (const auto& [fd, _] : _initial_fds) {
    if (fds.find(fd) == fds.end()) return nullopt;
  }

  // Keep track of any substitutions we have to make for tempfile names
  map<string, string> substitutions;

  // Loop over arguments to check for matches
  for (size_t i = 0; i < other_args.size(); i++) {
    // Are the arguments an exact match? If so, we still have a match
    if (other_args[i] == _args[i]) continue;

    // Are the mismatched arguments both temporary file paths?
    if (other_args[i].substr(0, 5) == "/tmp/" && _args[i].substr(0, 5) == "/tmp/") {
      // Great. Do we expect to find specific content in the temporary file?
      auto expected_iter = _previous_run._tempfile_expected_content.find(_args[i]);
      if (expected_iter != _previous_run._tempfile_expected_content.end()) {
        // Yes. Check to see if there's a match. First try to get the new tempfile artifact
        auto result = env::getRootDir()->resolve(nullptr, other_args[i].substr(1), NoAccess);

        // If we didn't get an artifact, bail
        if (!result.isSuccess()) return nullopt;

        // Check the content
        if (!expected_iter->second->matches(result.getArtifact()->peekContent())) {
          // The candidate tempfile does not have the content this command expects, so do not match
          return nullopt;
        }
      }

      // TODO: Check for expected metadata too. This is less likely to invalidate a match, but could
      // be helpful.

      // Yes. We can considuer this a match as long as we substitute the new command's temp path
      substitutions.emplace(_args[i], other_args[i]);

    } else {
      // No. This child does not match
      return nullopt;
    }
  }

  // Return the required temporary file name substitutions
  return substitutions;
}

/// Get the content inputs to this command
const Command::InputList& Command::getInputs() noexcept {
  ASSERT(options::track_inputs_outputs)
      << "Requested inputs from command when input/output tracking is off. Set "
         "options::track_inputs_outputs to true for this command.";
  return _previous_run._inputs;
}

/// Get the content outputs from this command
const Command::OutputList& Command::getOutputs() noexcept {
  ASSERT(options::track_inputs_outputs)
      << "Requested inputs from command when input/output tracking is off. Set "
         "options::track_inputs_outputs to true for this command.";
  return _previous_run._outputs;
}
