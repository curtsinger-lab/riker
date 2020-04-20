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

/*
 * Currently, Env only tracks the latest version stored at each path. If we instead keep track of
 * the whole sequence of versions, we can scan back through this sequence and find the oldest
 * version that still satisfies whatever predicate we're checking (e.g. access, contents, metadata).
 * This will be useful for accesses in directories that are modified, since we don't have to depend
 * on the latest versions, just the version that added whatever entry we're looking at. It also
 * could work similarly for metadata.
 *
 * One odd detail comes up when there are several different commands that seem to create suitable
 * versions of an artifact. We would need to encode this as a dependency on any one of those
 * commands rather than a specific one. That works out just fine, but when we go to mark the
 * commands that have to run, we now have multiple options to try. We could default to the oldest
 * version that works, but that could run more commands than choosing some other version.
 *
 * We could try ALL options for these alternative command dependencies. That gets particularly
 * expensive when there are multiple dependencies on sets of commands. If one command has two sets
 * of alternative command dependencies, maybe we could try looking at their intersection? Although,
 * if the command(s) in the intersection of those alternative sets are particularly expensive, it
 * coudl still be cheaper to run depend on two other commands.
 *
 * This is a problem to solve later. But, it could be an interesting thing to discuss in the paper
 * if it yields any interesting optimization opportunities.
 */

class Env {
 public:
  /// Default constructor
  Env() = default;

  /// Set the currently-executing command
  void startCommand(shared_ptr<Command> cmd) { _commands.push(cmd); }

  /// Finish the current command by popping it from the command stack
  void finishCommand() { _commands.pop(); }

  /**
   * Check if all entries in this environment match what is on the actual filesystem. This should be
   * run after emulating all commands. This check will look to see if the products of a build are
   * in place, and if not, create the necessary dependencies to replace them.
   *
   * \param marked   A set of commands. This method will add any commands whose output does not
   *                 appear on the filesystem to this set.
   */
  void checkFinalState(set<shared_ptr<Command>>& marked);

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
