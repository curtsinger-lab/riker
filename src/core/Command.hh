#pragma once

#include <array>
#include <cstdio>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <cereal/access.hpp>

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "util/UniqueID.hh"

class Tracer;

using std::array;
using std::endl;
using std::make_shared;
using std::map;
using std::ostream;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;
using std::weak_ptr;

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
  // Default constructor for deserialization
  friend class cereal::access;
  Command() = default;

  /// Create a new command
  Command(string exe, vector<string> args, map<int, FileDescriptor> initial_fds) :
      _exe(exe), _args(args), _initial_fds(initial_fds) {}

 public:
  /// Create a command to invoke the provided buildfile
  static shared_ptr<Command> createRootCommand(map<int, FileDescriptor> fds);

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /// Get a command's unique ID
  int getID() const { return _id; }

  /// Get a short, printable name for this command
  string getShortName() const;

  /// Get the full name for this command
  string getFullName() const;

  /// Get the list of traced steps this command runs
  const list<shared_ptr<Step>>& getSteps() const { return _steps; }

  /// Get the list of this command's children
  const list<shared_ptr<Command>>& getChildren() const { return _children; }

  /// Check if this command has never run
  bool neverRun() const { return _steps.size() == 0; }

  /// Run this command, or skip it and descend to its children if a run is unnecessary
  void run(const set<shared_ptr<Command>>& to_run, Tracer& tracer);

  /**
   * Check the state of this command and its descendants' inputs. This process populates the command
   * dependency graph, and adds any commands with changed inputs to the given set.
   *
   * \param env     The environment used to emulate filesystem actions in each command's trace, and
   *                to build the dependency edges used for rebuild planning.
   * \param result  The set of commands with at least one changed input.
   */
  void checkInputs(Env& env, set<shared_ptr<Command>>& result);

  /// Get the path to the executable file this command runs
  const string& getExecutable() const { return _exe; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }

  /********* Command Tracing Operations **********/

  /// The command accesses an artifact by path.
  /// This function returns a shared_ptr<Ref>,
  /// Most access() calls will *not* have side-effects, but some will:
  ///  - O_CREAT was specified, and the file did not exist before this call
  ///  - O_TRUNC was specified, and the file existed before this call
  shared_ptr<Reference> access(string path, AccessFlags flags);

  /// This command creates a reference to a new pipe
  shared_ptr<Reference> pipe();

  /// This command requires that a reference resolves to an artifact without failure
  void isOK(shared_ptr<Reference> ref);

  /// This command requires that a reference fails to resolve with a specific error
  void isError(shared_ptr<Reference> ref, int err);

  /// This command accesses the metadata for an artifact
  void metadataMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command accesses the contents of an artifact
  void contentsMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command sets the metadata for an artifact
  void setMetadata(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command sets the contents of an artifact
  void setContents(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command starts another command
  shared_ptr<Command> launch(string exe, vector<string> args, map<int, FileDescriptor> fds);

  /****** Rebuild Methods ******/

  /// This command needs output from command c before it can run
  void needs(shared_ptr<Command> c) { _needs.insert(c); }

  /// This command produces output that is used by command c, so rerunning this command triggers a
  /// rerun of c.
  void triggers(shared_ptr<Command> c) { _triggers.insert(c); }

  /// Mark this command to record that it must rerun. If the marking is new, propagate marking to
  /// all children, dependencies, and commands who depend on this one. Marked commands are added
  /// to the result set
  void mark(set<shared_ptr<Command>>& marked);

  /// Build a set of commands that includes only the ancestor commands that must be rerun
  void getMarkedAncestors(set<shared_ptr<Command>>& marked);

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) {
    return o << "[Command " << c.getID() << " " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) { return o << *c; }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Command& c, const uint32_t version);

 private:
  /// A unique ID assigned to this command for log readability
  UniqueID<Command> _id;

  /// The executable file this command runs
  string _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptors that should be opened prior to running this command
  map<int, FileDescriptor> _initial_fds;

  /// The steps performed by this command
  list<shared_ptr<Step>> _steps;

  /// The list of this command's children, in order of creation
  list<shared_ptr<Command>> _children;

  /***** Transient Data (not serialized) *****/

  /// Track all the unique metadata checks made during tracing
  set<pair<shared_ptr<Reference>, ArtifactVersion>> _metadata_checks;

  /// Track all the unique content checks made during tracing
  set<pair<shared_ptr<Reference>, ArtifactVersion>> _contents_checks;

  /// The set of commands that must run to produce inputs for this command
  /// Note: if this command requires output from another command, but we have cached copies of all
  /// dependencies, then that command will NOT appear in this set.
  set<shared_ptr<Command>> _needs;

  /// The set of commands that depend on this command's output
  /// Note: this set always includes the full set of output users, regardless of caching. If this
  /// command must run, it will mark as must_run all of these output users as well.
  set<shared_ptr<Command>> _triggers;

  /// Has this command already been marked for rerunning?
  bool _marked = false;
};
