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
 private:
  Rebuild(shared_ptr<Command> root) : _root(root) {}

 public:
  // Disallow Copy
  Rebuild(const Rebuild&) = delete;
  Rebuild& operator=(const Rebuild&) = delete;

  // Allow Move
  Rebuild(Rebuild&&) = default;
  Rebuild& operator=(Rebuild&&) = default;

  /// Create a rebuild plan for an existing build trace
  static Rebuild create(shared_ptr<Command> root);

  /// Run the rebuild
  void run();

  /// Get the artifact a reference resolves to
  shared_ptr<Artifact> getArtifact(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                   bool created = false);

  /// Print information about the rebuild state
  ostream& print(ostream& o) const;

  /// Output stream printing
  friend ostream& operator<<(ostream& o, const Rebuild& r) { return r.print(o); }

  /// Record the necessary dependencies for when command c accesses artifact a
  void recordDependency(shared_ptr<Command> c, shared_ptr<Artifact> a);

 private:
  /// Run or emulate a command from this rebuild
  void runCommand(shared_ptr<Command> c, Tracer& tracer);

  /// Check a command and its descendants to see if any inputs have changed
  void findChanges(shared_ptr<Command> c);

  /// Check to see if any files remaining in the environment match the filesystem state
  void checkFinalState();

  /// Check if a reference to the actual filesystem yields the expected metadata
  bool checkFilesystemMetadata(shared_ptr<Access> ref, shared_ptr<Version> v);

  /// Check if a reference to the actual filesystem yields the expected contents
  bool checkFilesystemContents(shared_ptr<Access> ref, shared_ptr<Version> v);

 private:
  /// The root command for the build
  shared_ptr<Command> _root;

  /// The environment that tracks artifacts during the checking phase of the rebuild
  Env _check_env;

  /// Track commands with changed inputs
  set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// A map of artifacts found on the filesystem
  map<ino_t, pair<string, shared_ptr<Artifact>>> _artifacts;

  /// A map of pipes
  map<shared_ptr<Pipe>, shared_ptr<Artifact>> _pipes;
};
