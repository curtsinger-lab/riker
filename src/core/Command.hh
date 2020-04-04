#pragma once

#include <cstddef>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <variant>
#include <vector>

#include "core/Action.hh"
#include "core/Artifact.hh"
#include "core/Predicate.hh"
#include "core/Ref.hh"
#include "ui/log.hh"
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
using std::variant;
using std::vector;
using std::weak_ptr;

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
  using Step = variant<shared_ptr<Ref>, shared_ptr<Predicate>, shared_ptr<Action>>;

 private:
  /**
   * Create a new command with a specified parent. This constructor is private, and is used only by
   * the createChild() method below.
   */
  Command(string exe, vector<string> args, shared_ptr<Command> parent) :
      _id(next_id++), _depth(parent->_depth + 1), _exe(exe), _args(args), _parent(parent) {}

 public:
  /// Create a new root command, which has no parent.
  Command(string exe, vector<string> args) : _id(next_id++), _depth(0), _exe(exe), _args(args) {}

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

  /// Create a child of this command
  shared_ptr<Command> createChild(string exe, vector<string> args);

  /// Add an int edge from a file version to this command. Return true if this is a new edge.
  bool addInput(Artifact::VersionRef f);

  /// Add an output edge from this command to a file version. Return true if this is a new edge.
  bool addOutput(Artifact::VersionRef f);

  /// Run this command, or skip it and descend to its children if a run is unnecessary
  void run(Tracer& tracer);

  /// Clean up the graph by pruning unneeded edges and nodes.
  /// If this returns true, the parent command can prune this command entirely.
  bool prune();

  /// Add this command and its children to a graphviz output object
  void drawGraph(Graphviz& g);

  /// Get the path to the executable file this command runs
  const string& getExecutable() const { return _exe; }

  /// Get the list of arguments this command was started with
  const vector<string>& getArguments() const { return _args; }

  /// Get this Command's children
  const vector<shared_ptr<Command>>& getChildren() const { return _children; }

  /// Get the set of file descriptors set up at the start of this command's run
  const map<int, shared_ptr<Ref>>& getInitialFDs() const { return _initial_fds; }

  /// Print a Command to an output stream
  friend ostream& operator<<(ostream& o, const Command& c) {
    return o << "[Command " << c._id << " " << c.getShortName() << "]";
  }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Command* c) { return o << *c; }

  shared_ptr<Ref> addReference(Ref::Flags flags = {}) {
    shared_ptr<Ref> r(new Ref(shared_from_this(), flags));
    _references.push_back(r);
    return r;
  }

  shared_ptr<Ref> addReference(string path, Ref::Flags flags = {}) {
    shared_ptr<Ref> r(new Ref(shared_from_this(), path, flags));
    _references.push_back(r);
    return r;
  }

  /// Record a file descriptor available on startup for this child
  shared_ptr<Ref> inheritReference(int fd, shared_ptr<Ref> ref) {
    // Create a new anonymous reference from the existing reference
    shared_ptr<Ref> new_ref(new Ref(shared_from_this(), ref));

    // Record the initial fd
    _initial_fds.emplace(fd, new_ref);

    // Record the reference itself
    _references.push_back(new_ref);

    return new_ref;
  }
  
  /// Set up the stdin, stdout, stderr references
  void addStandardReferences() {
    _initial_fds.emplace(0, addReference({.r = true})->resolvesTo(Artifact::stdin));
    _initial_fds.emplace(1, addReference({.w = true})->resolvesTo(Artifact::stdout));
    _initial_fds.emplace(2, addReference({.w = true})->resolvesTo(Artifact::stderr));
  }

  /********* New methods and types for command tracking **********/

  /// The command accesses an artifact by path.
  /// This function returns a shared_ptr<Ref>, 
  /// Most access() calls will *not* have side-effects, but some will:
  ///  - O_CREAT was specified, and the file did not exist before this call
  ///  - O_TRUNC was specified, and the file existed before this call
  shared_ptr<Ref> access(string path, Ref::Flags flags, shared_ptr<Artifact> f = shared_ptr<Artifact>()) {
    shared_ptr<Ref> r(new Ref(shared_from_this(), path, flags));
    r->resolvesTo(f);
    
    // TODO: if f exists and O_TRUNC is set in flags, this access creates a new version of the file

    _steps.push_back(r);
    return r;
  }

  /// This command requires that a reference resolves to an artifact without failure
  void isOK(shared_ptr<Ref> ref) {
    _steps.push_back(make_shared<Predicate::IsOK>(ref));
  }

  /// This command requires that a reference fails to resolve with a specific error
  void isError(shared_ptr<Ref> ref, int err) {
    _steps.push_back(make_shared<Predicate::IsError>(ref, err));
  }

  /// This command accesses the metadata for an artifact
  void metadataMatch(shared_ptr<Ref> ref) {
    // Log a read to the artifact first, which triggers the creation of a version if needed
    // TODO: remove this once versioning is updated
    auto a = ref->getArtifact();
    a->readBy(shared_from_this());

    // Record the dependency on metadata
    _steps.push_back(make_shared<Predicate::MetadataMatch>(ref, a->getLatestVersion()));
  }

  /// This command accesses the contents of an artifact
  void contentsMatch(shared_ptr<Ref> ref) {
    // Log a read to the artifact first, which triggers the creation of a version if needed
    // TODO: remove this once versioning is updated
    auto a = ref->getArtifact();
    a->readBy(shared_from_this());

    _steps.push_back(make_shared<Predicate::ContentsMatch>(ref, a->getLatestVersion()));
  }

  /// This command sets the contents of an artifact
  void setContents(shared_ptr<Ref> ref) {
    // Log a write to the artifact first, which triggers the creation of a version if needed
    // TODO: remove this once versioning is updated
    auto a = ref->getArtifact();
    a->writtenBy(shared_from_this());

    _steps.push_back(make_shared<Action::SetContents>(ref, a->getLatestVersion()));
  }

  /// This command starts another command
  void launch(shared_ptr<Command> cmd) {
    _steps.push_back(make_shared<Action::Launch>(cmd));
  }

  /// Print the abstract trace of this command (and its children) to an output stream
  void printTrace(ostream& o) const {
    // Print this command's name
    o << this << endl;

    // Print the trace for this command
    for (auto& s : _steps) {
      if (std::holds_alternative<shared_ptr<Ref>>(s)) {
        o << "  " << std::get<shared_ptr<Ref>>(s) << endl;
      } else if (std::holds_alternative<shared_ptr<Predicate>>(s)) {
        o << "  " << std::get<shared_ptr<Predicate>>(s) << endl;
      } else if (std::holds_alternative<shared_ptr<Action>>(s)) {
        o << "  " << std::get<shared_ptr<Action>>(s) << endl;
      }
    }

    // Print traces for all child commands
    for (auto& child : _children) {
      child->printTrace(o);
    }
  }

 private:
  /// A unique ID assigned to this command for log readability
  size_t _id;

  /// The depth of this command in the command tree
  size_t _depth;

  /// The executable file this command runs
  string _exe;

  /// The arguments passed to this command on startup
  vector<string> _args;

  /// The file descriptors that should be opened prior to running this command
  map<int, shared_ptr<Ref>> _initial_fds;

  /// Artifact versions read by this command
  set<Artifact::VersionRef> _inputs;

  /// Artifact versions written by this command
  set<Artifact::VersionRef> _outputs;

  /// This command's parent
  weak_ptr<Command> _parent;

  /// A list of this command's children
  vector<shared_ptr<Command>> _children;

  /// All of this command's references
  list<shared_ptr<Ref>> _references;

  /// The steps performed by this command
  list<Step> _steps;

  /// A static counter used to assign command IDs
  static size_t next_id;
};
