#pragma once

#include <cstddef>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include "core/Artifact.hh"
#include "core/Path.hh"
#include "core/Ref.hh"
#include "ui/log.hh"
#include "ui/options.hh"

class Graphviz;
class Tracer;

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

  void show() const {
    WARN << this;
    for (auto& r : _references) {
      WARN << "  " << r;
    }
  }

  ~Command() { show(); }

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

  /// A static counter used to assign command IDs
  static size_t next_id;
};
