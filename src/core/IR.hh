#pragma once

#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include <fcntl.h>
#include <unistd.h>

#include <cereal/access.hpp>

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"
#include "ui/log.hh"
#include "util/UniqueID.hh"

using std::map;
using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int { SUCCESS = 0 };

class Reference;

/**
 * A Command's actions are tracked as a sequence of Steps, each corresponding to some operation or
 * dependency we observed the last time a command executed.
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

  /**
   * Evaluate this build step in a hypothetical build environment. If the result of this build step
   * is the same as the recorded outcome, return true. Otherwise return false.
   * \param env   A map from paths to artifact versions placed at those paths
   * \returns true if the outcome is unchanged, or false if the build step should be rerun
   */
  virtual bool eval(map<string, ArtifactVersion>& env) = 0;

  /// Get the reference used by this Step, if any. Used for printing.
  virtual shared_ptr<Reference> getReference() const { return nullptr; }

  /// Print this Step to an output stream
  virtual ostream& print(ostream& o) const = 0;

  /// Stream print wrapper for Step references
  friend ostream& operator<<(ostream& o, const Step& s) { return s.print(o); }

  /// Stream print wrapper for Step pointers
  friend ostream& operator<<(ostream& o, const Step* s) { return o << *s; }

 private:
  UniqueID<Step> _id;
};

/**
 * Any time a command makes a reference to an artifact we will record it with an IR step that is a
 * subclass of Reference. References do not necessarily resolve to artifacts (they could fail) but
 * we can encode predicates about the outcome of a reference.
 */
class Reference : public Step {
 public:
  class Pipe;
  class Access;

  /// Get the path this reference uses, if it has one
  virtual optional<string> getPath() = 0;

  /// Evaluating a reference never updates the environment, and is not a predicate, so there's
  /// nothing to do here
  virtual bool eval(map<string, ArtifactVersion>& env) override { return true; }

  /// Get the result of making this reference again, and return it
  virtual int checkAccess() = 0;

  /// Get the short name for this reference
  string getName() const { return "r" + std::to_string(getID()); }
};

/// Create a reference to a new pipe
class Reference::Pipe : public Reference {
 public:
  /// Pipes do not have a path
  virtual optional<string> getPath() override { return nullopt; }

  /// Pipes are always created successfully
  virtual int checkAccess() override { return SUCCESS; }

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Pipe& p, const uint32_t version);
};

/// Access a filesystem path with a given set of flags
class Reference::Access : public Reference {
  // Default constructor for deserialization
  friend class cereal::access;
  Access() = default;

 public:
  /// Create an access reference to a path with given flags
  Access(string path, AccessFlags flags) : _path(path), _flags(flags) {}

  /// Get the flags used to create this reference
  const AccessFlags& getFlags() const { return _flags; }

  /// Get the path this access call uses to reach an artifact
  virtual optional<string> getPath() override { return _path; }

  /// Check the outcome of an access
  virtual int checkAccess() override;

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Access& a, const uint32_t version);

 private:
  string _path;        //< The filesystem path that was accessed
  AccessFlags _flags;  //< The relevant flags for the access
};

/**
 * Predicates allow us to encode a command's dependencies. We will check to see whether these
 * predicates still hold true prior to a rebuild; any time a command has at least one failing
 * predicate, we know we have to rerun that command.
 *
 * There are several types of predicates:
 * - REFERECE_RESULT(r : Reference, rc : int)
 * - METADATA_MATCH(r : Reference, v : ArtifactVersion)
 * - CONTENTS_MATCH(r : Reference, v : ArtifactVersion)
 */
class Predicate : public Step {
 public:
  class ReferenceResult;
  class MetadataMatch;
  class ContentsMatch;
};

/**
 * Making a reference produced a particular result (error code or success)
 */
class Predicate::ReferenceResult : public Predicate {
  // Default constructor for deserialization
  friend class cereal::access;
  ReferenceResult() = default;

 public:
  /// Create a REFERENCE_RESULT predicate
  ReferenceResult(shared_ptr<Reference> ref, int rc) : _ref(ref), _rc(rc) {}

  virtual shared_ptr<Reference> getReference() const override { return _ref; }

  /// Return true if the given reference evaluates to a different result
  virtual bool eval(map<string, ArtifactVersion>& env) override;

  /// Print a REFERENCE_RESULT predicate
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, ReferenceResult& p, const uint32_t version);

 private:
  shared_ptr<Reference> _ref;  //< The reference whose outcome we depend on
  int _rc;                     //< The result of that reference
};

/**
 * Require that the metadata accessed through a reference matches that of an artifact version
 */
class Predicate::MetadataMatch : public Predicate {
  // Default constructor for deserialization
  friend class cereal::access;
  MetadataMatch() = default;

 public:
  /// Create a METADATA_MATCH predicate
  MetadataMatch(shared_ptr<Reference> ref, ArtifactVersion version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  virtual shared_ptr<Reference> getReference() const override { return _ref; }

  /// Get the expected artifact version
  ArtifactVersion getVersion() const { return _version; }

  /// Check if this predicate has changed
  virtual bool eval(map<string, ArtifactVersion>& env) override;

  /// Print a METADATA_MATCH predicate
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, MetadataMatch& p, const uint32_t version);

 private:
  shared_ptr<Reference> _ref;  //< The reference being examined
  ArtifactVersion _version;    //< The artifact version whose metadata the reference must match
};

/**
 * Require that the contents accessed through a reference match that of an artifact version
 */
class Predicate::ContentsMatch : public Predicate {
  // Default constructor for deserialization
  friend class cereal::access;
  ContentsMatch() = default;

 public:
  /// Create a CONTENTS_MATCH predicate
  ContentsMatch(shared_ptr<Reference> ref, ArtifactVersion version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  virtual shared_ptr<Reference> getReference() const override { return _ref; }

  /// Get the expected artifact version
  ArtifactVersion getVersion() const { return _version; }

  /// Check if this predicate has changed
  virtual bool eval(map<string, ArtifactVersion>& env) override;

  /// Print a CONTENTS_MATCH predicate
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, ContentsMatch& p, const uint32_t version);

 private:
  shared_ptr<Reference> _ref;  //< The reference being examined
  ArtifactVersion _version;    //< The artifact version whose contents the reference must match
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
 public:
  class Launch;
  class SetMetadata;
  class SetContents;
};

/**
 * A Launch action creates a new command, which inherits some (possibly empty)
 * set of references from its parent.
 */
class Action::Launch : public Action {
  // Default constructor for deserialization
  friend class cereal::access;
  Launch() = default;

 public:
  /// Create a LAUNCH action
  Launch(shared_ptr<Command> cmd) : _cmd(cmd) {}

  /// Get the command this action launches
  shared_ptr<Command> getCommand() const { return _cmd; }

  /// Launch actions always evaluate successfully, with no effect on the environment
  virtual bool eval(map<string, ArtifactVersion>& env) override { return true; }

  /// Print a LAUNCH action
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Launch& a, const uint32_t version);

 private:
  shared_ptr<Command> _cmd;  //< The command that is being launched
};

/**
 * A SetMetadata action indicates that a command set the metadata for an artifact.
 */
class Action::SetMetadata : public Action {
  // Default constructor for deserialization
  friend class cereal::access;
  SetMetadata() = default;

 public:
  /// Create a SET_METADATA action
  SetMetadata(shared_ptr<Reference> ref, ArtifactVersion version) : _ref(ref), _version(version) {}

  /// Get the reference used for this action
  virtual shared_ptr<Reference> getReference() const override { return _ref; }

  /// Get the artifact version that is put in place
  ArtifactVersion getVersion() const { return _version; }

  /// Check whether this action's outcome has changed
  virtual bool eval(map<string, ArtifactVersion>& env) override;

  /// Print a SET_METADATA action
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, SetMetadata& a, const uint32_t version);

 private:
  shared_ptr<Reference> _ref;  //< The reference used for this action
  ArtifactVersion _version;    //< The artifact version with the metadata written by this action
};

/**
 * A SetContents action records that a command set the contents of an artifact.
 */
class Action::SetContents : public Action {
  // Default constructor for deserialization
  friend class cereal::access;
  SetContents() = default;

 public:
  /// Create a SET_CONTENTS action
  SetContents(shared_ptr<Reference> ref, ArtifactVersion version) : _ref(ref), _version(version) {}

  /// Get the reference used for this action
  virtual shared_ptr<Reference> getReference() const override { return _ref; }

  /// Get the artifact version that is put in place
  ArtifactVersion getVersion() const { return _version; }

  /// Check whether this action's outcome has changed
  virtual bool eval(map<string, ArtifactVersion>& env) override;

  /// Print a SET_CONTENTS action
  virtual ostream& print(ostream& o) const override;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, SetContents& a, const uint32_t version);

 private:
  shared_ptr<Reference> _ref;  //< The reference used for this action
  ArtifactVersion _version;    //< The artifact version with the contents written by this action
};
