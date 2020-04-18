#pragma once

#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"

using std::make_shared;
using std::map;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;

class Command;
class Reference;

class Env {
 public:
  // Default constructor
  Env() = default;

  // Default virtual destructor
  virtual ~Env() = default;

  /**
   * Check the result of an access in this environment or a preceding linked environment. Return
   * true if the result matches the expected value, or false otherwise.
   *
   * \param ref       The reference the command is attempting to access
   * \param expected  The expected result code from making this access (SUCCESS or an error code)
   * \returns true if the access matched the expected outcome, otherwise false
   */
  virtual bool checkAccess(shared_ptr<Reference> ref, int expected) = 0;

  /**
   * Check if a reference made in this environment resolves to an artifact with the expected
   * metadata. Return true if the metadata matches, or false otherwise.
   *
   * \param ref The reference to look up
   * \param v   The expected version to compare to
   * \returns true if the metadata matches, otherwise false
   */
  virtual bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) = 0;

  /**
   * Check if a reference made in this environment resolves to an artifact with the expected
   * contents. Return true if the contents match, or false otherwise.
   *
   * \param ref The reference to look up
   * \param v   The expected version to compare to
   * \returns true if the contents match, otherwise false
   */
  virtual bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVersion v) = 0;
};

/// A BaseEnv is an environment that performs all checks against the actual filesystem state
class BaseEnv : public Env {
 public:
  // Overrides for check methods documented in Env
  virtual bool checkAccess(shared_ptr<Reference> ref, int expected) override;
  virtual bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;
  virtual bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;
};

/// A CommandEnv captures the effects of an emulated command.
class CommandEnv : public Env {
 private:
  /// Create an environment for a command that links to an existing environment
  CommandEnv(shared_ptr<Command> cmd, shared_ptr<Env> link) : _cmd(cmd), _link(link) {}

 public:
  /// Create an environment for a root command
  CommandEnv(shared_ptr<Command> cmd) : _cmd(cmd), _link(make_shared<BaseEnv>()) {}

  // Overrides for check methods documented in Env
  virtual bool checkAccess(shared_ptr<Reference> ref, int expected) override;
  virtual bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;
  virtual bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;

  /// Set the metadata for a referenced artifact in this environment
  void setMetadata(shared_ptr<Reference> ref, ArtifactVersion v);

  /// Set the contents for a referenced artifact in this environment
  void setContents(shared_ptr<Reference> ref, ArtifactVersion v);

 private:
  /// The command that executes in this environment
  shared_ptr<Command> _cmd;

  /// Track all the modifications the running command makes to the environment here
  map<string, ArtifactVersion> _entries;

  /// A reference to the next environment where lookup is performed
  shared_ptr<Env> _link;
};
