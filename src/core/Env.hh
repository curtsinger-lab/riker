#pragma once

#include <map>
#include <memory>
#include <set>
#include <stack>
#include <string>
#include <utility>

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"

using std::make_shared;
using std::map;
using std::pair;
using std::set;
using std::shared_ptr;
using std::stack;
using std::string;

class Command;
class Access;
class Reference;

class Env {
 public:
  /// Default constructor
  Env() = default;

  /// Set the currently-executing command
  void startCommand(shared_ptr<Command> cmd) { _commands.push(cmd); }

  /// Finish the current command by popping it from the command stack
  void finishCommand() { _commands.pop(); }

  /**
   * Check the result of an access in this environment. If there is no entry, fall back to looking
   * in the actual filesystem. Return true if the result matches the expected value.
   *
   * \param ref       The reference being accessed
   * \param expected  The expected result code from making this access (SUCCESS or an error code)
   * \returns true if the access matched the expected outcome, otherwise false
   */
  bool checkAccess(shared_ptr<Reference> ref, int expected);

  /**
   * Check the result of an access against only the actual filesystem without looking in this
   * environment. This is still an instance method because we may add caching for repeated calls.
   *
   * Parameters are the same as checkAccess, except the reference must be of type Access
   */
  bool checkFilesystemAccess(shared_ptr<Access> ref, int expected);

  /**
   * Check if a reference made in this environment resolves to an artifact with the expected
   * metadata. Return true if the metadata matches, or false otherwise.
   *
   * \param ref The reference to look up
   * \param v   The expected version to compare to
   * \returns true if the metadata matches, otherwise false
   */
  bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v);

  /**
   * Check if a reference to the real filesystem resolves with the same metadata as a saved artifact
   * version. Return true if the metadata matches, or false otherwise.
   *
   * Parameters are the same as checkMetadataMatch, except the reference must be of type Access
   */
  bool checkFilesystemMetadataMatch(shared_ptr<Access> ref, ArtifactVersion v);

  /**
   * Check if a reference made in this environment resolves to an artifact with the expected
   * contents. Return true if the contents match, or false otherwise.
   *
   * \param ref The reference to look up
   * \param v   The expected version to compare to
   * \returns true if the contents match, otherwise false
   */
  bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVersion v);

  /**
   * Check if a reference to the real filesystem resolves with the same contents as a saved artifact
   * version. Return true if the contents match, or false otherwise.
   *
   * Parameters are the same as checkContentsMatch, except the reference must be of type Access
   */
  bool checkFilesystemContentsMatch(shared_ptr<Access> ref, ArtifactVersion v);

  /**
   * Set the metadata for a referenced artifact in this environment.
   *
   * \param ref   The reference where the metadata should be set
   * \param v     The artifact version whose metadata is being written to the environment
   */
  void setMetadata(shared_ptr<Reference> ref, ArtifactVersion v);

  /**
   * Set the contents for a referenced artifact in this environment.
   *
   * \param ref   The reference where the contents should be set
   * \param v     The artifact version whose contents are being written to the environment
   */
  void setContents(shared_ptr<Reference> ref, ArtifactVersion v);

 private:
  /// The command that is currently executing
  stack<shared_ptr<Command>> _commands;

  /// A map from paths to entries in this emulated view of the filesystem. Each entry holds the
  /// command that created the entry, and the artifact version that was put in place.
  map<string, pair<shared_ptr<Command>, ArtifactVersion>> _entries;
};
