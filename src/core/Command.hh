#pragma once

#include <cstdio>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include <cereal/access.hpp>

#include "core/Artifact.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "ui/options.hh"

class Graphviz;
class Tracer;

using std::endl;
using std::make_shared;
using std::map;
using std::ostream;
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

 public:
  /// Create a new root command, which has no parent.
  Command(string exe, vector<string> args, map<int, FileDescriptor> initial_fds) :
      _id(next_id++), _exe(exe), _args(args), _initial_fds(initial_fds) {}

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

  /// Run this command, or skip it and descend to its children if a run is unnecessary
  void run(Tracer& tracer);

  /// Add this command and its children to a graphviz output object
  void drawGraph(Graphviz& g);

  /// Get the path to the executable file this command runs
  const string& getExecutable() const { return _exe; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) {
    return o << "[Command " << c._id << " " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) { return o << *c; }

  /********* New methods and types for command tracking **********/

  /// The command accesses an artifact by path.
  /// This function returns a shared_ptr<Ref>,
  /// Most access() calls will *not* have side-effects, but some will:
  ///  - O_CREAT was specified, and the file did not exist before this call
  ///  - O_TRUNC was specified, and the file existed before this call
  shared_ptr<Reference> access(string path, Reference::Access::Flags flags) {
    auto ref = make_shared<Reference::Access>(path, flags);
    _steps.push_back(ref);

    // TODO: if f exists and O_TRUNC is set in flags, this access creates a new version of the file
    // TODO: if f does not exist and O_CREAT is set, this access adds an entry to the containing
    // directory

    return ref;
  }

  /// This command creates a reference to a new pipe
  shared_ptr<Reference> pipe() {
    auto ref = make_shared<Reference::Pipe>();
    _steps.push_back(ref);
    return ref;
  }

  /// This command requires that a reference resolves to an artifact without failure
  void isOK(shared_ptr<Reference> ref) { _steps.push_back(make_shared<Predicate::IsOK>(ref)); }

  /// This command requires that a reference fails to resolve with a specific error
  void isError(shared_ptr<Reference> ref, int err) {
    _steps.push_back(make_shared<Predicate::IsError>(ref, err));
  }

  /// This command accesses the metadata for an artifact
  void metadataMatch(shared_ptr<Reference> ref, Artifact::VersionRef v) {
    // Record the dependency on metadata
    _steps.push_back(make_shared<Predicate::MetadataMatch>(ref, v));
  }

  /// This command accesses the contents of an artifact
  void contentsMatch(shared_ptr<Reference> ref, Artifact::VersionRef v) {
    _steps.push_back(make_shared<Predicate::ContentsMatch>(ref, v));
  }

  /// This command sets the contents of an artifact
  void setContents(shared_ptr<Reference> ref, Artifact::VersionRef v) {
    _steps.push_back(make_shared<Action::SetContents>(ref, v));
  }

  /// This command starts another command
  void launch(shared_ptr<Command> cmd) { _steps.push_back(make_shared<Action::Launch>(cmd)); }

  /// Print the abstract trace of this command (and its children) to an output stream
  void printTrace(ostream& o) const {
    // Print this command's name
    o << this << endl;

    list<shared_ptr<Command>> children;

    // Print the trace for this command
    for (auto& s : _steps) {
      o << "  " << s << endl;

      // If this is a LAUNCH step, we will need to print the child command
      auto launch = std::dynamic_pointer_cast<Action::Launch>(s);
      if (launch) {
        children.push_back(launch->getCommand());
      }
    }

    // Print traces for all child commands
    for (auto& child : children) {
      child->printTrace(o);
    }
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Command& c);

 private:
  /// A unique ID assigned to this command for log readability
  size_t _id;

  /// The executable file this command runs
  string _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptors that should be opened prior to running this command
  map<int, FileDescriptor> _initial_fds;

  /// The steps performed by this command
  list<shared_ptr<Step>> _steps;

  /// A static counter used to assign command IDs
  static size_t next_id;
};
