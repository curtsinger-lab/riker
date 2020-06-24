#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>

#include <sys/stat.h>
#include <sys/types.h>

#include "build/Resolution.hh"
#include "core/AccessFlags.hh"
#include "util/UniqueID.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

class Artifact;
class Build;
class Command;
class ContentVersion;
class MetadataVersion;
class SymlinkVersion;
class Version;

// Set up a map from return codes to names
inline static map<int8_t, string> errors = {
    {SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
    {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
    {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}, {ENOTDIR, "ENOTDIR"}};

/**
 * A Command's actions are tracked as a sequence of Steps, each corresponding to some operation
 * or dependency we observed the last time a command executed.
 *
 * Step is the abstract parent class for all IR values.
 * All command steps fall into one of three categories:
 * - Reference: a reference to some artifact made by a command
 * - Predicate: a statement about a reference that was true on the example build
 * - Action: a modification to system state performed by the command
 */
class Step : public std::enable_shared_from_this<Step> {
 public:
  /// Use a default virtual destructor
  virtual ~Step() noexcept = default;

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept = 0;

  /// Try to cast this IR step to an instance of a specific IR step type.
  template <class T>
  shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Const equivalent
  template <class T>
  shared_ptr<const T> as() const noexcept {
    return std::dynamic_pointer_cast<const T>(shared_from_this());
  }

  /// Print this Step to an output stream
  virtual ostream& print(ostream& o) const noexcept = 0;

  /// Stream print wrapper for Step references
  friend ostream& operator<<(ostream& o, const Step& s) noexcept { return s.print(o); }

  /// Stream print wrapper for Step pointers
  friend ostream& operator<<(ostream& o, const Step* s) noexcept { return o << *s; }

 private:
  SERIALIZE_EMPTY();
};

/**
 * Any time a command makes a reference to an artifact we will record it with an IR step that is a
 * subclass of Reference. References do not necessarily resolve to artifacts (they could fail) but
 * we can encode predicates about the outcome of a reference.
 */
class Reference : public Step {
 public:
  /// Get the unique ID for this reference
  size_t getID() const noexcept { return _id; }

  /// Get the short name for this reference
  string getName() const noexcept { return "r" + std::to_string(getID()); }

  /// Record the result of this access from a trace
  void expectResult(int rc) noexcept { _expected_rc = rc; }

  /// Get the expected result of this access
  int getExpectedResult() const noexcept { return _expected_rc; }

  /// Get the result of resolving this reference
  Resolution getResolution() const noexcept { return _res; }

  /// Check if this reference is resolved
  bool isResolved() const noexcept { return _res; }

  /// Get the artifact this reference resolved to
  shared_ptr<Artifact> getArtifact() const noexcept { return _res; }

  /// A sub-type can report the result of resolving this artifact using this method
  void resolvesTo(Resolution res) noexcept { _res = res; }

 private:
  /// The expected result from this access
  int _expected_rc = SUCCESS;

  SERIALIZE(BASE(Step), _expected_rc);

  /****** Transient Fields ******/

  /// Assign a unique ID to each reference
  UniqueID<Reference> _id;

  /// The result of resolving this reference
  Resolution _res;
};

/// Create a reference to a new pipe
class Pipe final : public Reference {
 public:
  /// Create a pipe
  Pipe() noexcept = default;

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const noexcept override {
    o << getName() << " = PIPE()";
    if (isResolved()) {
      // Print the artifact this pipe resolves to
      o << " -> " << getArtifact();
    } else {
      int rc = getResolution();
      o << " expect " << errors[rc];
    }
    return o;
  }

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(Reference));
};

/// Create a reference to a new symlnk
class Symlink final : public Reference {
 public:
  // Create a symlink
  Symlink(fs::path target) noexcept : _target(target) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const noexcept override {
    o << getName() << " = SYMLINK(" << _target << ")";
    if (isResolved()) {
      // Print the artifact this pipe resolves to
      o << " -> " << getArtifact();
    } else {
      int rc = getResolution();
      o << " expect " << errors[rc];
    }
    return o;
  }

 private:
  fs::path _target;

  // Specify fields for serialization
  Symlink() = default;
  SERIALIZE(BASE(Reference), _target);
};

/// Access a filesystem path with a given set of flags
class Access final : public Reference {
 public:
  /// Create an access reference to a path with given flags
  Access(shared_ptr<Access> base, fs::path path, AccessFlags flags) noexcept :
      _base(base), _path(path), _flags(flags) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Get the access that serves as the base for this one
  const shared_ptr<Access>& getBase() const noexcept { return _base; }

  /// Get the path of this reference, relative to the base access
  fs::path getRelativePath() const noexcept { return _path; }

  /// Get the path this ACCESS reference uses
  fs::path getFullPath() const noexcept {
    if (_base) {
      return _base->getFullPath() / _path;
    } else {
      return _path;
    }
  }

  /// Get the flags used to create this reference
  const AccessFlags& getFlags() const noexcept { return _flags; }

  /// Open this reference
  int open() const noexcept;

  /// Call lstat on this reference
  tuple<struct stat, int> lstat() const noexcept;

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << getName() << " = ACCESS(" << getFullPath() << ", [" << getFlags() << "]) -> "
             << errors[getExpectedResult()];
  }

 private:
  /// The base used to resolve this reference, typically either cwd or root.
  shared_ptr<Access> _base;

  /// The path being accessed
  fs::path _path;

  /// The relevant flags for the access
  AccessFlags _flags;

  // Create default constructor and specify fields for serialization
  Access() = default;
  SERIALIZE(BASE(Reference), _base, _path, _flags);
};

/**
 * A command with this predicate expects an artifact reached through a reference to match a
 * particular version.
 */
template <class VersionType>
class Match final : public Step {
 public:
  /// Create a MATCH predicate
  Match(shared_ptr<Reference> ref, shared_ptr<VersionType> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a MATCH predicate
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "MATCH(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Reference> _ref;        //< The reference being examined
  shared_ptr<VersionType> _version;  //< The expected metadata

  // Create default constructor and specify fields for serialization
  Match() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * A Launch action creates a new command, which inherits some (possibly empty)
 * set of references from its parent.
 */
class Launch final : public Step {
 public:
  /// Create a LAUNCH action
  Launch(shared_ptr<Command> cmd) noexcept : _cmd(cmd) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a LAUNCH action
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "LAUNCH(" << _cmd << ")";
  }

 private:
  shared_ptr<Command> _cmd;  //< The command that is being launched

  // Create default constructor and specify fields for serialization
  Launch() = default;
  SERIALIZE(BASE(Step), _cmd);
};

/**
 * A Join action records when a parent command joins with a specific child, and saves the exit
 * status from that child.
 */
class Join final : public Step {
 public:
  /// Create a JOIN action
  Join(shared_ptr<Command> cmd, int exit_status) noexcept : _cmd(cmd), _exit_status(exit_status) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a JOIN action
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "JOIN(" << _cmd << ", " << _exit_status << ")";
  }

 private:
  shared_ptr<Command> _cmd;  //< The command that was joined with
  int _exit_status;          //< The exit status of the child

  // Create default constructor and specify fields for serialization
  Join() = default;
  SERIALIZE(BASE(Step), _cmd, _exit_status);
};

/**
 * A command writes a version to an artifact reached via a reference
 */
template <class VersionType>
class Apply final : public Step {
 public:
  /// Create a SET action
  Apply(shared_ptr<Reference> ref, shared_ptr<VersionType> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a SET action
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "APPLY(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<VersionType> _version;

  // Create default constructor and specify fields for serialization
  Apply() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};
