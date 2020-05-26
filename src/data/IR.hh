#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "data/AccessFlags.hh"
#include "data/Version.hh"
#include "util/UniqueID.hh"
#include "util/serializer.hh"

using std::dynamic_pointer_cast;
using std::map;
using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int { SUCCESS = 0 };

class Command;
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

  /// Get the unique ID for this IR node
  size_t getID() const { return _id; }

  /// Print this Step to an output stream
  virtual ostream& print(ostream& o) const = 0;

  /// Stream print wrapper for Step references
  friend ostream& operator<<(ostream& o, const Step& s) { return s.print(o); }

  /// Stream print wrapper for Step pointers
  friend ostream& operator<<(ostream& o, const Step* s) { return o << *s; }

 private:
  UniqueID<Step> _id;

  SERIALIZE();
};

/**
 * Any time a command makes a reference to an artifact we will record it with an IR step that is a
 * subclass of Reference. References do not necessarily resolve to artifacts (they could fail) but
 * we can encode predicates about the outcome of a reference.
 */
class Reference : public Step {
 public:
  /// Get the short name for this reference
  string getName() const { return "r" + std::to_string(getID()); }

 private:
  SERIALIZE(BASE(Step));
};

/// Create a reference to a new pipe
class Pipe : public Reference {
 public:
  /// Create a pipe
  Pipe() = default;

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const override;

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(Reference));
};

/// Access a filesystem path with a given set of flags
class Access : public Reference {
 public:
  /// Create an access reference to a path with given flags
  Access(string path, AccessFlags flags) : _path(path), _flags(flags) {}

  /// Get the path this ACCESS reference uses
  const string& getPath() const { return _path; }

  /// Get the flags used to create this reference
  const AccessFlags& getFlags() const { return _flags; }

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const override;

 private:
  string _path;        //< The filesystem path that was accessed
  AccessFlags _flags;  //< The relevant flags for the access

  // Create default constructor and specify fields for serialization
  Access() = default;
  SERIALIZE(BASE(Reference), _path, _flags);
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
class ReferenceResult : public Predicate {
 public:
  /// Create a REFERENCE_RESULT predicate
  ReferenceResult(shared_ptr<Reference> ref, int rc) : _ref(ref), _rc(rc) {}

  /// Get the reference this predicate examines
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected result of the reference
  int getResult() const { return _rc; }

  /// Print a REFERENCE_RESULT predicate
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;  //< The reference whose outcome we depend on
  int _rc;                     //< The result of that reference

  // Create default constructor and specify fields for serialization
  ReferenceResult() = default;
  SERIALIZE(BASE(Predicate), _ref, _rc);
};

/**
 * Require that the metadata accessed through a reference matches that of an artifact version
 */
class MetadataMatch : public Predicate {
 public:
  /// Create a METADATA_MATCH predicate
  MetadataMatch(shared_ptr<Reference> ref, shared_ptr<Version> version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected artifact version
  shared_ptr<Version> getVersion() const { return _version; }

  /// Print a METADATA_MATCH predicate
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;    //< The reference being examined
  shared_ptr<Version> _version;  //< The artifact version whose metadata the reference must match

  // Create default constructor and specify fields for serialization
  MetadataMatch() = default;
  SERIALIZE(BASE(Predicate), _ref, _version);
};

/**
 * Require that the contents accessed through a reference match that of an artifact version
 */
class ContentsMatch : public Predicate {
 public:
  /// Create a CONTENTS_MATCH predicate
  ContentsMatch(shared_ptr<Reference> ref, shared_ptr<Version> version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected artifact version
  shared_ptr<Version> getVersion() const { return _version; }

  /// Print a CONTENTS_MATCH predicate
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;    //< The reference being examined
  shared_ptr<Version> _version;  //< The artifact version whose contents the reference must match

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
class Launch : public Action {
 public:
  /// Create a LAUNCH action
  Launch(shared_ptr<Command> cmd) : _cmd(cmd) {}

  /// Get the command this action launches
  shared_ptr<Command> getCommand() const { return _cmd; }

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
class SetMetadata : public Action {
 public:
  /// Create a SET_METADATA action
  SetMetadata(shared_ptr<Reference> ref, shared_ptr<Version> version) :
      _ref(ref), _version(version) {}

  shared_ptr<Reference> getReference() const { return _ref; }

  shared_ptr<Version> getVersion() const { return _version; }

  /// Print a SET_METADATA action
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<Version> _version;

  // Create default constructor and specify fields for serialization
  SetMetadata() = default;
  SERIALIZE(BASE(Action), _ref, _version);
};

/**
 * A SetContents action records that a command set the contents of an artifact.
 */
class SetContents : public Action {
 public:
  /// Create a SET_CONTENTS action
  SetContents(shared_ptr<Reference> ref, shared_ptr<Version> version) :
      _ref(ref), _version(version) {}

  shared_ptr<Reference> getReference() const { return _ref; }

  shared_ptr<Version> getVersion() const { return _version; }

  /// Print a SET_CONTENTS action
  virtual ostream& print(ostream& o) const override;

 private:
  shared_ptr<Reference> _ref;
  shared_ptr<Version> _version;

  // Create default constructor and specify fields for serialization
  SetContents() = default;
  SERIALIZE(BASE(Action), _ref, _version);
};
