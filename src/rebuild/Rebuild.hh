#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <sys/types.h>

#include "data/AccessFlags.hh"
#include "data/Command.hh"
#include "data/InitialFD.hh"
#include "rebuild/Artifact.hh"
#include "rebuild/Env.hh"

using std::map;
using std::ostream;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

class Access;
class Reference;
class Pipe;
class Tracer;

/// This class captures all of the logic and state required to plan a rebuild.
class Rebuild {
 public:
  /// Create a rebuild plan
  Rebuild(shared_ptr<Command> root);

  // Disallow Copy
  Rebuild(const Rebuild&) = delete;
  Rebuild& operator=(const Rebuild&) = delete;

  // Allow Move
  Rebuild(Rebuild&&) = default;
  Rebuild& operator=(Rebuild&&) = default;

  /// Run the rebuild
  void run();

  /// Check if a specific command must rerun
  bool mustRerun(shared_ptr<Command> c) const;

  /// Record the necessary dependencies for when command c accesses artifact a
  void addDependency(shared_ptr<Command> c, shared_ptr<Artifact> a);

  /// Print information about the rebuild state
  ostream& print(ostream& o) const;

  /// Output stream printing
  friend ostream& operator<<(ostream& o, const Rebuild& r) { return r.print(o); }

 private:
  /// Run or emulate a command from this rebuild
  void runCommand(shared_ptr<Command> c, Tracer& tracer);

  /// Check a command and its descendants to see if any inputs have changed
  void findChanges(shared_ptr<Command> c);

  /// Check to see if any files remaining in the environment match the filesystem state
  void checkFinalState();

  /// Mark a command for rerun, and propagate that marking to its dependencies/dependents
  void mark(shared_ptr<Command> c);

 private:
  /// The root command for the build
  shared_ptr<Command> _root;

  /// This environment resolves and tracks artifacts during the checking and re-execution phases
  Env _env;

  /// Track commands with changed inputs
  set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// All commands that will rerun
  set<shared_ptr<Command>> _rerun;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Map command that consumes uncached input -> commands that produce that input
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;
};
