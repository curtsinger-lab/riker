#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "core/AccessFlags.hh"
#include "core/FileDescriptor.hh"
#include "util/serializer.hh"

using std::list;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

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
  static shared_ptr<Command> createRootCommand() noexcept;

  /// Create a new command
  Command(string exe, vector<string> args, map<int, FileDescriptor> initial_fds,
          shared_ptr<Access> initial_cwd, shared_ptr<Access> initial_root) noexcept :
      _exe(exe),
      _args(args),
      _initial_fds(initial_fds),
      _initial_cwd(initial_cwd),
      _initial_root(initial_root) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) noexcept = default;
  Command& operator=(Command&&) noexcept = default;

  /// Get a short, printable name for this command
  string getShortName() const noexcept;

  /// Get the full name for this command
  string getFullName() const noexcept;

  /// Get the working directory where this command is started
  shared_ptr<Access> getInitialWorkingDirectory() const noexcept { return _initial_cwd; }

  /// Get the root directory in effect when this command is started
  shared_ptr<Access> getInitialRoot() const noexcept { return _initial_root; }

  /// Get the list of traced steps this command runs
  const list<shared_ptr<Step>>& getSteps() const noexcept { return _steps; }

  /// Get the list of this command's children
  const list<shared_ptr<Command>>& getChildren() const noexcept { return _children; }

  /// Check if this command has never run
  bool neverRun() const noexcept { return _steps.size() == 0; }

  /// Reset the record for this command in preparation for re-execution
  void reset() noexcept {
    _steps.clear();
    _children.clear();
  }

  /// Get the path to the executable file this command runs
  const string& getExecutable() const noexcept { return _exe; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const noexcept { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, FileDescriptor>& getInitialFDs() const noexcept { return _initial_fds; }

  /// Emulate the steps of this command as part of a particular build
  void emulate(Build& build) noexcept;

  /********* Command Tracing Operations **********/

  /// This command accesses a path relative to some base reference, using the given flags
  shared_ptr<Access> access(fs::path path, AccessFlags flags, shared_ptr<Access> base) noexcept;

  /// This command accesses an already-constructed reference using new flags
  shared_ptr<Access> access(shared_ptr<Access> a, AccessFlags flags) noexcept;

  /// This command creates a pipe
  shared_ptr<Pipe> pipe() noexcept;

  /// This command depends on the metadata of a referenced artifact
  void metadataMatch(shared_ptr<Reference> ref) noexcept;

  /// This command depends on the contents of a referenced artifact
  void contentsMatch(shared_ptr<Reference> ref) noexcept;

  /// This command sets the metadata of a referenced artifact
  void setMetadata(shared_ptr<Reference> ref) noexcept;

  /// This command sets the contents of a referenced artifact
  void setContents(shared_ptr<Reference> ref) noexcept;

  /// This command launches a child command
  shared_ptr<Command> launch(string exe, const vector<string>& args,
                             const map<int, FileDescriptor>& fds, shared_ptr<Access> cwd,
                             shared_ptr<Access> root) noexcept;

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) noexcept {
    return o << "[Command " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) noexcept { return o << *c; }

 private:
  /// The executable file this command runs
  string _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptor table at the start of this command's execution
  map<int, FileDescriptor> _initial_fds;

  /// A reference to the directory where this command is started
  shared_ptr<Access> _initial_cwd;

  /// A reference to the root directory in effect when this command is started
  shared_ptr<Access> _initial_root;

  /// The steps performed by this command
  list<shared_ptr<Step>> _steps;

  /// The list of this command's children, in order of creation
  list<shared_ptr<Command>> _children;

  // Create default constructor and specify fields for serialization
  Command() = default;
  SERIALIZE(_exe, _args, _initial_fds, _initial_cwd, _initial_root, _steps, _children);
};
