#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>

#include "core/Artifact.hh"

using std::map;
using std::ostream;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;

class Access;
class Build;
class Command;
class Reference;
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
  static Rebuild create(Build& b);

  /// Run the rebuild
  void run();

  /// Get the artifact a reference resolves to
  shared_ptr<Artifact> getArtifact(shared_ptr<Reference> ref);

  /// Print information about the rebuild state
  ostream& print(ostream& o) const;

  /// Output stream printing
  friend ostream& operator<<(ostream& o, const Rebuild& r) { return r.print(o); }

 private:
  /// Run or emulate a command from this rebuild
  void runCommand(shared_ptr<Command> c, Tracer& tracer);

  /// Check if a specific command must rerun
  bool mustRerun(shared_ptr<Command> c) { return _rerun.find(c) != _rerun.end(); }

  /// Check a command and its descendants to see if any inputs have changed
  void findChanges(shared_ptr<Command> c);

  /// Check to see if any files remaining in the environment match the filesystem state
  void checkFinalState();

  /// Mark a command for rerun. If this is a new mark, propagate the marking to its dependents
  void mark(shared_ptr<Command> c);

  /// Check if a command's access resolves as expected in the environment for this rebuild
  bool checkAccess(shared_ptr<Command> c, shared_ptr<Reference> ref, int expected);

  /// Check if a command's dependency on metadata resolves to the expected version
  bool checkMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref, ArtifactVersion v);

  /// Check if a command's dependency on contents resolves to the expected version
  bool checkContents(shared_ptr<Command> c, shared_ptr<Reference> ref, ArtifactVersion v);

  /// Command c sets file metadata in the rebuild environment
  void setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref, ArtifactVersion v);

  /// Command c sets file contents in the rebuild environment
  void setContents(shared_ptr<Command> c, shared_ptr<Reference> ref, ArtifactVersion v);

  /// Check if an access resolves as expected against the actual filesystem state
  bool checkFilesystemAccess(shared_ptr<Access> ref, int expected);

  /// Check if a reference to the actual filesystem yields the expected metadata
  bool checkFilesystemMetadata(shared_ptr<Access> ref, ArtifactVersion v);

  /// Check if a reference to the actual filesystem yields the expected contents
  bool checkFilesystemContents(shared_ptr<Access> ref, ArtifactVersion v);

 private:
  /// The root command for the build
  shared_ptr<Command> _root;

  /// A map from paths to entries in an emulated view of the filesystem. Each entry maps a path
  /// to a specific artifact version that was placed there by some command.
  map<string, ArtifactVersion> _entries;

  /// Track commands with changed inputs
  set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Record edges where one command requires output from other commands. These edges are only
  /// created when we do not have a cached copy of the output to stage in.
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;

  /// Record edges where one command produces output that is used by other commands.
  /// These edges exist whether or not we have cached copies of output.
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Track the commands that have been marked for rerunning
  set<shared_ptr<Command>> _rerun;

  /// A map of artifacts found on the filesystem
  map<ino_t, shared_ptr<Artifact>> _artifacts;
};
