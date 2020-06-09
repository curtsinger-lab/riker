#pragma once

#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include "core/AccessFlags.hh"
#include "core/FileDescriptor.hh"
#include "util/serializer.hh"

using std::list;
using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

class Access;
class Artifact;
class Build;
class BuildObserver;
class Pipe;
class Reference;
class Step;

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
  friend class RebuildPlanner;

 public:
  /// Create a command to invoke the provided buildfile
  static shared_ptr<Command> createRootCommand();

  /// Create a new command
  Command(string exe, vector<string> args, map<int, FileDescriptor> initial_fds) :
      _exe(exe), _args(args), _initial_fds(initial_fds) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

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
  const map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }

  /// Emulate the steps of this command as part of a particular build
  void emulate(Build& build);

  /********* Command Tracing Operations **********/

  /// This command accesses a path
  shared_ptr<Access> access(string path, AccessFlags flags);

  /// This command creates a pipe
  shared_ptr<Pipe> pipe();

  /// This command depends on the outcome of a reference
  void referenceResult(const shared_ptr<Reference>& ref, int result);

  /// This command depends on the metadata of a referenced artifact
  void metadataMatch(const shared_ptr<Reference>& ref);

  /// This command depends on the contents of a referenced artifact
  void contentsMatch(const shared_ptr<Reference>& ref);

  /// This command sets the metadata of a referenced artifact
  void setMetadata(const shared_ptr<Reference>& ref);

  /// This command sets the contents of a referenced artifact
  void setContents(const shared_ptr<Reference>& ref);

  /// This command launches a child command
  shared_ptr<Command> launch(string exe, vector<string> args, map<int, FileDescriptor> fds);

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) {
    return o << "[Command " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) { return o << *c; }

 private:
  /// The executable file this command runs
  string _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptor table at the start of this command's execution
  map<int, FileDescriptor> _initial_fds;

  /// The steps performed by this command
  list<shared_ptr<Step>> _steps;

  /// The list of this command's children, in order of creation
  list<shared_ptr<Command>> _children;

  // Create default constructor and specify fields for serialization
  Command() = default;
  SERIALIZE(_exe, _args, _initial_fds, _steps, _children);

  /****** Transient Data ******/

  // Keep a record of the references that are used to write metadata and contents.
  // Future writes through a stored reference can be skipped if:
  // 1. This command is the last writer of the artifact's content/metadata
  // 2. The same reference was last used to write the artifact's content/metadata
  // 3. The artifact's content/metadata has not been accessed since its last write
  set<shared_ptr<Reference>> _metadata_writes;
  set<shared_ptr<Reference>> _content_writes;

  // Keep a record of references used to read metadata and contents and the observed versions.
  // Future reads through a stored reference can be skipped if the artifact still has the same
  // version for its content/metadata.
  map<shared_ptr<Reference>, shared_ptr<MetadataVersion>> _metadata_reads;
  map<shared_ptr<Reference>, shared_ptr<ContentVersion>> _content_reads;
};
