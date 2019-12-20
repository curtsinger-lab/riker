#pragma once

#include <cstddef>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include "core/Artifact.hh"
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

/*
 * A command uses a collection of references to artifacts. Normal
 *
 * Each path reference is a string path, as well as any flags that influence path resolution.
 * These flags are:
 *   nofollow: determines whether a reference will access a symlink or the linked path
 *   are there others?
 *
 * A command also has one or more interactions with each path reference.
 * Interaction types are:
 *   Resolve: follow this path to an artifact with a set of permissions. This could fail.
 *            Resolution could create the artifact, depending on flags to open.
 *   Read: read the artifact this path resolves to. We'll record the version that was read.
 *   Write: write the artifact this path resolves to. We'll record the version this creates.
 *   Truncate: truncate the artifact this path resolves to. We'll record the version this creates.
 *   Link: link an artifact at this path
 *   Unlink: unlink the artifact at this path.
 *
 * At the end of an entire build, we'll look at how every path reference in every command resolves
 * and save that final state. On a future build, we will check to see if a command's references
 * match their final states from the last build. If they do, the command does not need to run.
 *
 * For input-only files, this logic is reasonable; none of the inputs are changed by the build, so
 * the final state is also the initial state. Changing inputs to a command would rerun the
 * command.
 *
 * For output-only files, we'd rerun a command if its output no longer exists (e.g. the user
 * removed it). We could potentially skip this and just link the file back into place.
 *
 * The tricky case is files that are both modified and read by the build. One command may create a
 * file, a second may read it, and a third may remove it. If the build process is idempotent,
 * rerunning the entire build would bring the file to whatever state it was in at the end of the
 * last build. Of course builds may not actually be idempotent; a build might append a message to
 * a log each time it is run. The log will finish in a different state than it started on each
 * build, but we don't want that change to trigger a rebuild. Recording the final state of a
 * reference means we will only trigger builds when some artifact is changed outside of the build
 * process; changes to artifacts or paths by the build itself will not induce future builds.
 */

/**
 * Representation of a path to a file.
 * Includes both the string part of the path, and whether links are followed. If the final element
 * of the path is a symlink and the path is marked nofollow, the path will resolve to the symlink
 * and not its destination.
 */
class Path {
 public:
  /// Create a path
  explicit Path(string path, bool follow_links = true) : _path(path), _follow_links(follow_links) {}

  /// Get the string portion of the path
  const string& getPath() const { return _path; }

  /// Check if the path follows links
  bool followLinks() const { return _follow_links; }

  /// Check if two paths are equal
  bool operator==(const Path& other) const {
    return std::tie(_path, _follow_links) == std::tie(other._path, other._follow_links);
  }

  /// Check if one path is less than another
  bool operator<(const Path& other) const {
    return std::tie(_path, _follow_links) < std::tie(other._path, other._follow_links);
  }

  /// Check if one path is greater than another
  bool operator>(const Path& other) const {
    return std::tie(_path, _follow_links) > std::tie(other._path, other._follow_links);
  }

  /// Make paths printable
  friend ostream& operator<<(ostream& o, const Path& p) {
    return o << "[Path " << p._path << (p._follow_links ? "" : " nofollow") << "]";
  }

 private:
  /// The string portion of a path
  string _path;

  /// Does this path follow links, o
  bool _follow_links;
};

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
  Command(string exe, vector<string> args, map<int, shared_ptr<Ref>> initial_fds,
          shared_ptr<Command> parent) :
      _id(next_id++),
      _depth(parent->_depth + 1),
      _exe(exe),
      _args(args),
      _initial_fds(initial_fds),
      _parent(parent) {}

 public:
  /// Create a new root command, which has no parent.
  Command(string exe, vector<string> args) :
      _id(next_id++),
      _depth(0),
      _exe(exe),
      _args(args),
      _initial_fds({{0, make_shared<Ref>(O_RDONLY, false)->resolvesTo(Artifact::stdin)},
                    {1, make_shared<Ref>(O_WRONLY, false)->resolvesTo(Artifact::stdout)},
                    {2, make_shared<Ref>(O_WRONLY, false)->resolvesTo(Artifact::stderr)}}) {}

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
  shared_ptr<Command> createChild(string exe, vector<string> args, map<int, shared_ptr<Ref>> fds);

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

  /// Track the interactions this command
  class Interactions {
    friend class Command;

   private:
    /// Create a new set of interactions for a given command
    Interactions(shared_ptr<Command> command) : _command(command) {}

   public:
    /// Print Interactions to an output stream
    friend ostream& operator<<(ostream& o, const Interactions& i) {
      return o << "    [Interactions]";
    }

    /// Print Interactions* to an output stream
    friend ostream& operator<<(ostream& o, const Interactions* i) { return o << *i; }

   private:
    /// The Command that contains this Interactions tracking object
    weak_ptr<Command> _command;
  };

  /// Record a reference this command makes to a particular path
  /// This returns an Interactions object, which the tracer can use to record this command's
  /// interactions with artifacts through that path.
  shared_ptr<Interactions> references(string path, bool follow_links = true) {
    Path p(path, follow_links);

    // Do we already have a reference for this path?
    auto iter = _references.find(p);
    if (iter == _references.end()) {
      // If not, add an entry and return it
      shared_ptr<Interactions> result(new Interactions(shared_from_this()));
      _references.emplace_hint(iter, p, result);
      return result;
    }

    // Return the existing interactions object
    return iter->second;
  }

  void show() const {
    WARN << this;
    for (auto& r : _references) {
      WARN << "  " << r.first;
      WARN << r.second;
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

  /// The set of paths this command references, and the interactions with each
  map<Path, shared_ptr<Interactions>> _references;

  /// A static counter used to assign command IDs
  static size_t next_id;
};
