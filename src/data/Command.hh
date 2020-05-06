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
  // Default constructor for deserialization
  friend class cereal::access;
  Command() = default;

  /// Create a new command
  Command(string exe, vector<string> args, map<int, InitialFD> initial_fds) :
      _exe(exe), _args(args), _initial_fds(initial_fds) {}

 public:
  /// Create a command to invoke the provided buildfile
  static shared_ptr<Command> createRootCommand();

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
  }

  /// Get the path to the executable file this command runs
  const string& getExecutable() const { return _exe; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, InitialFD>& getInitialFDs() const { return _initial_fds; }

  /********* Command Tracing Operations **********/

 private:
  friend class Rebuild;
  /// The command accesses an artifact by path.
  /// This function returns a shared_ptr<Ref>,
  /// Most access() calls will *not* have side-effects, but some will:
  ///  - O_CREAT was specified, and the file did not exist before this call
  ///  - O_TRUNC was specified, and the file existed before this call
  shared_ptr<Reference> access(string path, AccessFlags flags);

  /// This command creates a reference to a new pipe
  shared_ptr<Reference> pipe();

  /// This command observes a reference resolve with a particular result
  void referenceResult(shared_ptr<Reference> ref, int result);

  /// This command accesses the metadata for an artifact
  void metadataMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command accesses the contents of an artifact
  void contentsMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command sets the metadata for an artifact
  void setMetadata(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command sets the contents of an artifact
  void setContents(shared_ptr<Reference> ref, shared_ptr<Artifact> a);

  /// This command starts another command
  shared_ptr<Command> launch(string exe, vector<string> args, map<int, InitialFD> fds);

 public:
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

  /// The file descriptor table at the start of this command's execution
  map<int, InitialFD> _initial_fds;

  /// The steps performed by this command
  list<shared_ptr<Step>> _steps;

  /// The list of this command's children, in order of creation
  list<shared_ptr<Command>> _children;

  /***** Transient Data (not serialized) *****/

  /// Track all the unique metadata checks made during tracing
  set<pair<shared_ptr<Reference>, shared_ptr<Version>>> _metadata_checks;

  /// Track all the unique content checks made during tracing
  set<pair<shared_ptr<Reference>, shared_ptr<Version>>> _contents_checks;
};
