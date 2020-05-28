#pragma once

#include <cstdint>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <cereal/access.hpp>

#include "data/AccessFlags.hh"
#include "data/InitialFD.hh"
#include "rebuild/Artifact.hh"
#include "util/UniqueID.hh"
#include "util/serializer.hh"

using std::list;
using std::map;
using std::ostream;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

class Reference;
class Step;
class Version;

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
  friend class Rebuild;
  friend class Artifact;

 public:
  /// Create a command to invoke the provided buildfile
  static shared_ptr<Command> createRootCommand();

  /// Create a new command
  Command(string exe, vector<string> args, map<int, InitialFD> initial_fds) :
      _exe(exe), _args(args), _initial_fds(initial_fds) {}

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

  /// Reset the record for this command in preparation for re-execution
  void reset() {
    _steps.clear();
    _children.clear();
    _metadata_checks.clear();
    _contents_checks.clear();
  }

  /// Get the path to the executable file this command runs
  const string& getExecutable() const { return _exe; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, InitialFD>& getInitialFDs() const { return _initial_fds; }

  /// This command accesses a path
  shared_ptr<Access> access(string path, AccessFlags flags);

  /// This command creates a pipe
  shared_ptr<Pipe> pipe();

  /// This command depends on the outcome of a reference
  void referenceResult(shared_ptr<Reference> ref, int result);

  /// This command depends on the metadata of a referenced artifact
  void metadataMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command depends on the contents of a referenced artifact
  void contentsMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command sets the metadata of a referenced artifact
  void setMetadata(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command sets the contents of a referenced artifact
  void setContents(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command launches a child command
  shared_ptr<Command> launch(string exe, vector<string> args, map<int, InitialFD> fds);

  /// Tell this command that it must rerun, and propagate that mark along the command graph
  void mark();

  /// Command c uses this command's output; c must rerun if this command runs
  void outputUsedBy(shared_ptr<Command> c) { _output_used_by.insert(c); }

  /// This command produces output that is used by command c, and cannot be reproduced
  /// Command c must rerun if this command runs
  void needsOutputFrom(shared_ptr<Command> c) { _needs_output_from.insert(c); }

  /// Check if this command has been marked for rerun
  bool mustRerun() const { return _rerun; }

  /********* Command Tracing Operations **********/

 private:
  /// Add a trace step to this command
  void addStep(shared_ptr<Step> s) { _steps.push_back(s); }

  /// Add a child to this command
  void addChild(shared_ptr<Command> c) { _children.push_back(c); }

  /// Record a metadata check performed by this command, and return true if it is new
  bool checkMetadataRequired(shared_ptr<Reference> ref, shared_ptr<Version> v) {
    auto [_, inserted] = _metadata_checks.emplace(ref, v);
    return inserted;
  }

  /// Record a contents check performed by this command, and return true if it is new
  bool checkContentsRequired(shared_ptr<Reference> ref, shared_ptr<Version> v) {
    auto [_, inserted] = _contents_checks.emplace(ref, v);
    return inserted;
  }

 public:
  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) {
    return o << "[Command " << c.getID() << " " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) { return o << *c; }

 private:
  /// A unique ID assigned to this command for log readability
  UniqueID<Command> _id;

  /// The executable file this command runs
  string _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptor table at the start of this command's execution
  map<int, InitialFD> _initial_fds;

  /// The steps performed by this command
  list<shared_ptr<Step>> _steps;

  /// The list of this command's children, in order of creation
  list<shared_ptr<Command>> _children;

  // Create default constructor and specify fields for serialization
  Command() = default;
  SERIALIZE(_exe, _args, _initial_fds, _steps, _children);

  /***** Transient Data (not serialized) *****/

  /// Track all the unique metadata checks made during tracing
  map<shared_ptr<Reference>, shared_ptr<Version>> _metadata_checks;

  /// Track all the unique content checks made during tracing
  map<shared_ptr<Reference>, shared_ptr<Version>> _contents_checks;

  /// Does this command have to rerun?
  bool _rerun = false;

  /// What commands does this command need output from?
  set<shared_ptr<Command>> _needs_output_from;

  /// What commands use this command's output?
  set<shared_ptr<Command>> _output_used_by;
};
