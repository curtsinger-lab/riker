#pragma once

#include <memory>
#include <ostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "core/Artifact.hh"

using std::ostream;
using std::pair;
using std::shared_ptr;
using std::string;
using std::unordered_map;
using std::unordered_set;

class Access;
class Build;
class Command;
class Reference;

/// This class captures all of the logic and state required to plan a rebuild.
class Rebuild {
 private:
  Rebuild() = default;

 public:
  // Disallow Copy
  Rebuild(const Rebuild&) = delete;
  Rebuild& operator=(const Rebuild&) = delete;

  // Allow Move
  Rebuild(Rebuild&&) = default;
  Rebuild& operator=(Rebuild&&) = default;

  /// Create a rebuild plan for an existing build trace
  static Rebuild create(Build& b);

  /// Print information about the rebuild state
  ostream& print(ostream& o);

  /// Check if a specific command must rerun
  bool mustRerun(shared_ptr<Command> c) { return _rerun.find(c) != _rerun.end(); }

 private:
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
  /// A map from paths to entries in an emulated view of the filesystem. Each entry tracks the
  /// command that created it, as well as the actual artifact version at the given path
  unordered_map<string, pair<shared_ptr<Command>, ArtifactVersion>> _entries;

  /// Track commands with changed inputs
  unordered_set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  unordered_set<shared_ptr<Command>> _output_needed;

  /// Record edges where one command requires output from other commands. These edges are only
  /// created when we do not have a cached copy of the output to stage in.
  unordered_map<shared_ptr<Command>, unordered_set<shared_ptr<Command>>> _needs_output_from;

  /// Record edges where one command produces output that is used by other commands.
  /// These edges exist whether or not we have cached copies of output.
  unordered_map<shared_ptr<Command>, unordered_set<shared_ptr<Command>>> _output_used_by;

  /// Track the commands that have been marked for rerunning
  unordered_set<shared_ptr<Command>> _rerun;
};
