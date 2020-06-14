#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>

#include <sys/stat.h>
#include <sys/types.h>

#include "core/AccessFlags.hh"
#include "util/UniqueID.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int8_t { SUCCESS = 0 };

class Artifact;
class Build;
class BuildObserver;
class Command;
class ContentVersion;
class MetadataVersion;
class Version;

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
class Step {
 public:
  /// Use a default virtual destructor
  virtual ~Step() noexcept = default;

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept = 0;

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

  /**
   * Resolve this reference within the context of a specific build
   * \param c     The command that performs the resolution
   * \param build The build that is running at the time of resolution
   */
  virtual void resolve(shared_ptr<Command> c, Build& build) noexcept = 0;

  /// Has this reference been resolved to an artifact?
  bool isResolved() const noexcept { return _artifact != nullptr; }

  /// Get the artifact this reference resolved to
  shared_ptr<Artifact> getArtifact() const noexcept {
    ASSERT(_artifact) << "Attempted to access unresolved reference " << this;
    return _artifact;
  }

  /// Get the result of trying to resolve this reference
  int getResult() const noexcept { return _rc; }

  /// A sub-type can report the result of resolving this artifact using this method
  void resolvesTo(shared_ptr<Artifact> artifact, int rc) noexcept {
    _artifact = artifact;
    _rc = rc;
  }

 private:
  /// The expected result from this access
  int _expected_rc = SUCCESS;

  SERIALIZE(BASE(Step), _expected_rc);

  /****** Transient Fields ******/

  /// Assign a unique ID to each reference
  UniqueID<Reference> _id;

  /// The artifact this reference resolved to
  shared_ptr<Artifact> _artifact;

  /// The result of resolving this reference
  int _rc;
};

/// Create a reference to a new pipe
class Pipe final : public Reference, public std::enable_shared_from_this<Pipe> {
 public:
  /// Create a pipe
  Pipe() noexcept = default;

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  virtual void resolve(shared_ptr<Command> c, Build& build) noexcept final;

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(Reference));
};

/// Access a filesystem path with a given set of flags
class Access final : public Reference, public std::enable_shared_from_this<Access> {
 public:
  /// Create an access reference to a path with given flags
  Access(shared_ptr<Access> base, string path, AccessFlags flags) noexcept :
      _base(base), _path(path), _flags(flags) {}

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  virtual void resolve(shared_ptr<Command> c, Build& build) noexcept final;

  shared_ptr<Access> withFlags(AccessFlags flags) noexcept {
    return shared_ptr<Access>(new Access(_base, _path, flags));
  }

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

  /// Call stat on this reference
  tuple<struct stat, int> stat() const noexcept;

  /// Call access() on this reference
  int access() const noexcept;

  /// Create an access relative to this reference
  shared_ptr<Access> get(string p, AccessFlags flags) noexcept {
    return shared_ptr<Access>(new Access(shared_from_this(), p, flags));
  }

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  /// The base used to resolve this reference, typically either cwd or root.
  shared_ptr<Access> _base;

  /// The path being accessed
  string _path;

  /// The relevant flags for the access
  AccessFlags _flags;

  // Create default constructor and specify fields for serialization
  Access() = default;
  SERIALIZE(BASE(Reference), _base, _path, _flags);
};

/**
 * Require that the metadata accessed through a reference matches that of an artifact version
 */
class MetadataMatch final : public Step, public std::enable_shared_from_this<MetadataMatch> {
 public:
  /// Create a METADATA_MATCH predicate
  MetadataMatch(shared_ptr<Reference> ref, shared_ptr<MetadataVersion> version) noexcept :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  const shared_ptr<Reference>& getReference() const noexcept { return _ref; }

  /// Get the expected artifact version
  const shared_ptr<MetadataVersion>& getVersion() const noexcept { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  /// Print a METADATA_MATCH predicate
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  shared_ptr<Reference> _ref;            //< The reference being examined
  shared_ptr<MetadataVersion> _version;  //< The expected metadata

  // Create default constructor and specify fields for serialization
  MetadataMatch() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * Require that the contents accessed through a reference match that of an artifact version
 */
class ContentsMatch final : public Step, public std::enable_shared_from_this<ContentsMatch> {
 public:
  /// Create a CONTENTS_MATCH predicate
  ContentsMatch(shared_ptr<Reference> ref, shared_ptr<ContentVersion> version) noexcept :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  const shared_ptr<Reference>& getReference() const noexcept { return _ref; }

  /// Get the expected artifact version
  const shared_ptr<ContentVersion>& getVersion() const noexcept { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  /// Print a CONTENTS_MATCH predicate
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  shared_ptr<Reference> _ref;           //< The reference being examined
  shared_ptr<ContentVersion> _version;  //< The expected contents

  // Create default constructor and specify fields for serialization
  ContentsMatch() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * A Launch action creates a new command, which inherits some (possibly empty)
 * set of references from its parent.
 */
class Launch final : public Step, public std::enable_shared_from_this<Launch> {
 public:
  /// Create a LAUNCH action
  Launch(shared_ptr<Command> cmd) noexcept : _cmd(cmd) {}

  /// Get the command this action launches
  shared_ptr<Command> getCommand() const noexcept { return _cmd; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  /// Print a LAUNCH action
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  shared_ptr<Command> _cmd;  //< The command that is being launched

  // Create default constructor and specify fields for serialization
  Launch() = default;
  SERIALIZE(BASE(Step), _cmd);
};

/**
 * A SetMetadata action indicates that a command set the metadata for an artifact.
 */
class SetMetadata final : public Step, public std::enable_shared_from_this<SetMetadata> {
 public:
  /// Create a SET_METADATA action
  SetMetadata(shared_ptr<Reference> ref, shared_ptr<MetadataVersion> version) noexcept :
      _ref(ref), _version(version) {}

  const shared_ptr<Reference>& getReference() const noexcept { return _ref; }

  const shared_ptr<MetadataVersion>& getVersion() const noexcept { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  /// Print a SET_METADATA action
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<MetadataVersion> _version;

  // Create default constructor and specify fields for serialization
  SetMetadata() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * A SetContents action records that a command set the contents of an artifact.
 */
class SetContents final : public Step, public std::enable_shared_from_this<SetContents> {
 public:
  /// Create a SET_CONTENTS action
  SetContents(shared_ptr<Reference> ref, shared_ptr<ContentVersion> version) noexcept :
      _ref(ref), _version(version) {}

  const shared_ptr<Reference>& getReference() const noexcept { return _ref; }

  const shared_ptr<ContentVersion>& getVersion() const noexcept { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept final;

  /// Print a SET_CONTENTS action
  virtual ostream& print(ostream& o) const noexcept final;

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<ContentVersion> _version;

  // Create default constructor and specify fields for serialization
  SetContents() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};
