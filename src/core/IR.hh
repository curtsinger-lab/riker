#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>

#include "core/AccessFlags.hh"
#include "util/UniqueID.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::ostream;
using std::pair;
using std::shared_ptr;
using std::string;

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
  virtual ~Step() = default;

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) = 0;

  /// Print this Step to an output stream
  virtual ostream& print(ostream& o) const = 0;

  /// Stream print wrapper for Step references
  friend ostream& operator<<(ostream& o, const Step& s) { return s.print(o); }

  /// Stream print wrapper for Step pointers
  friend ostream& operator<<(ostream& o, const Step* s) { return o << *s; }

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
  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Get the unique ID for this reference
  size_t getID() const { return _id; }

  /// Get the short name for this reference
  string getName() const { return "r" + std::to_string(getID()); }

  /**
   * Resolve this reference within the context of a specific build
   * \param c     The command that performs the resolution
   * \param build The build that is running at the time of resolution
   */
  virtual void resolve(shared_ptr<Command> c, Build& build) = 0;

  /// Has this reference been resolved to an artifact?
  bool isResolved() const { return _artifact != nullptr; }

  /// Get the artifact this reference resolved to
  shared_ptr<Artifact> getArtifact() const {
    ASSERT(_artifact) << "Attempted to access unresolved reference " << this;
    return _artifact;
  }

  /// Get the result of trying to resolve this reference
  int getResult() const { return _rc; }

 protected:
  /// A sub-type can report the result of resolving this artifact using this method
  void resolutionResult(shared_ptr<Artifact> a, int rc) {
    _artifact = a;
    _rc = rc;
  }

 private:
  /// Assign a unique ID to each reference
  UniqueID<Reference> _id;

  SERIALIZE(BASE(Step));

  /****** Transient Fields ******/

  /// The artifact this reference resolved to
  shared_ptr<Artifact> _artifact;

  /// The result of resolving this reference
  int _rc;
};

/// Create a reference to a new pipe
class Pipe final : public Reference, public std::enable_shared_from_this<Pipe> {
 public:
  /// Create a pipe
  Pipe() = default;

  virtual void resolve(shared_ptr<Command> c, Build& build) final;

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const override;

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(Reference));
};

/// Access a filesystem path with a given set of flags
class Access final : public Reference, public std::enable_shared_from_this<Access> {
 private:
  /// Create an access reference to a path with given flags
  Access(shared_ptr<Access> base, string path, AccessFlags flags) :
      _base(base), _path(path), _flags(flags) {}

 public:
  static shared_ptr<Access> createRoot(AccessFlags flags) {
    return shared_ptr<Access>(new Access(nullptr, "/", flags));
  }

  static shared_ptr<Access> createCwd(AccessFlags flags) {
    return shared_ptr<Access>(new Access(nullptr, ".", flags));
  }

  virtual void resolve(shared_ptr<Command> c, Build& build) final;

  shared_ptr<Access> withFlags(AccessFlags flags) {
    return shared_ptr<Access>(new Access(_base, _path, flags));
  }

  /// Get the path this ACCESS reference uses
  fs::path getPath() const {
    if (_base) {
      return _base->getPath() / _path;
    } else {
      return _path;
    }
  }

  /// Get the flags used to create this reference
  const AccessFlags& getFlags() const { return _flags; }

  /// Open this reference
  int open() const;

  /// Stat this reference
  pair<struct stat, int> stat() const;

  /// Call access() on this reference
  int access() const;

  /// Create an access relative to this reference
  shared_ptr<Access> get(string p, AccessFlags flags) {
    return shared_ptr<Access>(new Access(shared_from_this(), p, flags));
  }

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const override;

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
 * Predicates allow us to encode a command's dependencies. We will check to see whether these
 * predicates still hold true prior to a rebuild; any time a command has at least one failing
 * predicate, we know we have to rerun that command.
 *
 * There are several types of predicates:
 * - REFERECE_RESULT(r : Reference, rc : int)
 * - METADATA_MATCH(r : Reference, v : shared_ptr<Version>)
 * - CONTENTS_MATCH(r : Reference, v : shared_ptr<Version>)
 */
class Predicate : public Step {
 private:
  SERIALIZE(BASE(Step));
};

/**
 * Making a reference produced a particular result (error code or success)
 */
class ReferenceResult final : public Predicate,
                              public std::enable_shared_from_this<ReferenceResult> {
 public:
  /// Create a REFERENCE_RESULT predicate
  ReferenceResult(shared_ptr<Reference> ref, int8_t rc) : _ref(ref), _rc(rc) {}

  /// Get the reference this predicate examines
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected result of the reference
  int getResult() const { return _rc; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Print a REFERENCE_RESULT predicate
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;  //< The reference whose outcome we depend on
  int8_t _rc;                  //< The result of that reference

  // Create default constructor and specify fields for serialization
  ReferenceResult() = default;
  SERIALIZE(BASE(Predicate), _ref, _rc);
};

/**
 * Require that the metadata accessed through a reference matches that of an artifact version
 */
class MetadataMatch final : public Predicate, public std::enable_shared_from_this<MetadataMatch> {
 public:
  /// Create a METADATA_MATCH predicate
  MetadataMatch(shared_ptr<Reference> ref, shared_ptr<MetadataVersion> version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected artifact version
  shared_ptr<MetadataVersion> getVersion() const { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Print a METADATA_MATCH predicate
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;            //< The reference being examined
  shared_ptr<MetadataVersion> _version;  //< The expected metadata

  // Create default constructor and specify fields for serialization
  MetadataMatch() = default;
  SERIALIZE(BASE(Predicate), _ref, _version);
};

/**
 * Require that the contents accessed through a reference match that of an artifact version
 */
class ContentsMatch final : public Predicate, public std::enable_shared_from_this<ContentsMatch> {
 public:
  /// Create a CONTENTS_MATCH predicate
  ContentsMatch(shared_ptr<Reference> ref, shared_ptr<ContentVersion> version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected artifact version
  shared_ptr<ContentVersion> getVersion() const { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Print a CONTENTS_MATCH predicate
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;           //< The reference being examined
  shared_ptr<ContentVersion> _version;  //< The expected contents

  // Create default constructor and specify fields for serialization
  ContentsMatch() = default;
  SERIALIZE(BASE(Predicate), _ref, _version);
};

/**
 * An action describes a step taken by a command that could become visible to some other command.
 * If we are able to skip execution of a command (all its predicates match) we are responsible for
 * performing these actions on behalf of the skipped command.
 *
 * The types of actions are:
 * - LAUNCH(cmd : Command, inherited_refs : [Reference])
 * - SET_METADATA(r : Reference, v : Artifact::Version)
 * - SET_CONTENTS(r : Reference, v : Artifact::Version)
 */
class Action : public Step {
 private:
  SERIALIZE(BASE(Step));
};

/**
 * A Launch action creates a new command, which inherits some (possibly empty)
 * set of references from its parent.
 */
class Launch final : public Action, public std::enable_shared_from_this<Launch> {
 public:
  /// Create a LAUNCH action
  Launch(shared_ptr<Command> cmd) : _cmd(cmd) {}

  /// Get the command this action launches
  shared_ptr<Command> getCommand() const { return _cmd; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Print a LAUNCH action
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Command> _cmd;  //< The command that is being launched

  // Create default constructor and specify fields for serialization
  Launch() = default;
  SERIALIZE(BASE(Action), _cmd);
};

/**
 * A SetMetadata action indicates that a command set the metadata for an artifact.
 */
class SetMetadata final : public Action, public std::enable_shared_from_this<SetMetadata> {
 public:
  /// Create a SET_METADATA action
  SetMetadata(shared_ptr<Reference> ref, shared_ptr<MetadataVersion> version) :
      _ref(ref), _version(version) {}

  shared_ptr<Reference> getReference() const { return _ref; }

  shared_ptr<MetadataVersion> getVersion() const { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Print a SET_METADATA action
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<MetadataVersion> _version;

  // Create default constructor and specify fields for serialization
  SetMetadata() = default;
  SERIALIZE(BASE(Action), _ref, _version);
};

/**
 * A SetContents action records that a command set the contents of an artifact.
 */
class SetContents final : public Action, public std::enable_shared_from_this<SetContents> {
 public:
  /// Create a SET_CONTENTS action
  SetContents(shared_ptr<Reference> ref, shared_ptr<ContentVersion> version) :
      _ref(ref), _version(version) {}

  shared_ptr<Reference> getReference() const { return _ref; }

  shared_ptr<ContentVersion> getVersion() const { return _version; }

  /**
   * Emulate this IR step in a given environment
   * \param c   The command that contains the IR step
   * \param env The environment this step should be emulated in
   */
  virtual void emulate(shared_ptr<Command> c, Build& build) override;

  /// Print a SET_CONTENTS action
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<ContentVersion> _version;

  // Create default constructor and specify fields for serialization
  SetContents() = default;
  SERIALIZE(BASE(Action), _ref, _version);
};
