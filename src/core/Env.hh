#pragma once

#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>

#include "core/AccessFlags.hh"
#include "core/IR.hh"

using std::make_shared;
using std::map;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;

class Command;

class Env {
 public:
  // Default constructor
  Env() = default;

  // Default virtual destructor
  virtual ~Env() = default;

  /**
   * Check the result of an access in this environment or a preceding linked environment.
   * If the access matches the expected result, record this as a dependency on the environment where
   * the match was found. If not, the dependency has changed.
   *
   * \param ref       The reference the command is attempting to access
   * \param expected  The expected result code from making this access (SUCCESS or an error code)
   * \returns true if the access matched the expected outcome, otherwise false
   */
  virtual bool checkAccess(shared_ptr<Reference> ref, int expected) = 0;

  virtual bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) = 0;

  virtual bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVerson v) = 0;
};

/**
 * This class represents a filesystem environment where a command is run. This is used to keep track
 * of command inputs during the rebuild decision process, and as a record of what artifacts must be
 * staged in prior to running a command.
 *
 * Each environment is specific to a command. Any time we launch a new command, we start a new
 * environment. This makes it easy to track which files may change when a command is rerun; all of
 * the changes that command would make are recorded in a single environment. We can also quickly see
 * which other commands reference the entries in this environment. After a command has been
 * emulated, the parent command the launched it resumes with a new environment; this environment is
 * chained to the child command's environment, so the parent can see the files written by the child.
 */
class CommandEnv : public Env {
 private:
  /// Create an environment for a command that links to an existing environment
  CommandEnv(shared_ptr<Command> cmd, shared_ptr<Env> link) : _cmd(cmd), _link(link) {}

 public:
  /// Create an environment for a root command
  CommandEnv(shared_ptr<Command> cmd) : _cmd(cmd), _link(make_shared<BaseEnv>()) {}

  virtual bool checkAccess(shared_ptr<Reference> ref, int expected) override;

  virtual bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;

  virtual bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVerson v) override;

  void setMetadata(shared_ptr<Reference> ref, ArtifactVersion v);

  void setContents(shared_ptr<Reference> ref, ArtifactVersion v);

 private:
  /// The command that executes in this environment
  shared_ptr<Command> _cmd;

  /// Track all the modifications the running command makes to the environment here
  map<string, ArtifactVersion> _entries;

  /// A reference to the next environment where lookup is performed
  shared_ptr<Env> _link;
};

class BaseEnv : public Env {
 public:
  virtual bool checkAccess(shared_ptr<Reference> ref, int expected) override;
  virtual bool checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;
  virtual bool checkContentsMatch(shared_ptr<Reference> ref, ArtifactVersion v) override;
};
