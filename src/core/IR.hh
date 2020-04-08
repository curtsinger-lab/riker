#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>

#include <fcntl.h>

#include <cereal/access.hpp>

#include "core/Artifact.hh"
#include "core/UniqueID.hh"

using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;

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
 * A Reference is created any time a command refers to an artifact. This happens when commands open
 * files, but other cases (like creating pipes) will also need to be tracked.
 *
 * The types of references are:
 * - PIPE()
 * - ACCESS(<path>, <mode>)
 */
class Reference : public Step {
 public:
  class Pipe;
  class Access;

  /// Get the short name for this reference
  string getName() const { return "r" + std::to_string(getID()); }
};

/// Create a reference to a new pipe
class Reference::Pipe : public Reference {
 public:
  virtual ostream& print(ostream& o) const { return o << getName() << " = PIPE()"; }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Pipe& p);
};

/// Access a filesystem path with a given set of flags
class Reference::Access : public Reference {
  // Default constructor for deserialization
  friend class cereal::access;
  Access() = default;

 public:
  /// This struct encodes the flags specified when making an access to a particular reference
  struct Flags {
    bool r = false;          // Does the reference require read access?
    bool w = false;          // Does the reference require write access?
    bool x = false;          // Does the reference require execute access?
    bool nofollow = false;   // Does the reference resolve to a symlink rather than its target?
    bool truncate = false;   // Does the reference truncate the artifact's contents?
    bool create = false;     // Does the reference create an artifact if none exists?
    bool exclusive = false;  // Does the reference require creation? (must also be set with .create

    /// Create a Flags instance from the flags parameter to the open syscall
    static Flags fromOpen(int flags) {
      return {.r = (flags & O_RDONLY) == O_RDONLY || (flags & O_RDWR) == O_RDWR,
              .w = (flags & O_WRONLY) == O_WRONLY || (flags & O_RDWR) == O_RDWR,
              .nofollow = (flags & O_NOFOLLOW) == O_NOFOLLOW,
              .truncate = (flags & O_TRUNC) == O_TRUNC,
              .create = (flags & O_CREAT) == O_CREAT,
              .exclusive = (flags & O_EXCL) == O_EXCL};
    }

    /// Create a Flags instance from the mode and flags parameters to the access syscall
    static Flags fromAccess(int mode, int flags) {
      return {.r = (mode & R_OK) == R_OK,
              .w = (mode & W_OK) == W_OK,
              .x = (mode & X_OK) == X_OK,
              .nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW};
    }

    /// Create a Flags instance from the flags parameter to the stat syscall
    static Flags fromStat(int flags) {
      return {.nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW};
    }

    /// Print a Flags struct to an output stream
    friend ostream& operator<<(ostream& o, const Flags& f) {
      return o << (f.r ? 'r' : '-') << (f.w ? 'w' : '-') << (f.x ? 'x' : '-')
               << (f.nofollow ? " nofollow" : "") << (f.truncate ? " truncate" : "")
               << (f.create ? " create" : "") << (f.exclusive ? " exclusive" : "");
    }
  };

  /// Create an access reference to a path with given flags
  Access(string path, Flags flags) : _path(path), _flags(flags) {}

  /// Get the flags used to create this reference
  const Flags& getFlags() const { return _flags; }

  /// Print an access reference
  virtual ostream& print(ostream& o) const {
    return o << getName() << " = ACCESS(\"" << _path << "\", [" << getFlags() << "])";
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Access& a);

 private:
  string _path;  //< The filesystem path that was accessed
  Flags _flags;  //< The relevant flags for the access
};

/**
 * Predicates allow us to encode a command's dependencies. We will check to see whether these
 * predicates still hold true prior to a rebuild; any time a command has at least one failing
 * predicate, we know we have to rerun that command.
 *
 * There are several types of predicates:
 * - IS_OK(r : Reference)
 * - IS_ERROR(r : Reference, e : Error)
 * - METADATA_MATCH(r : Reference, v : Artifact::VersionRef)
 * - CONTENTS_MATCH(r : Reference, v : Artifact::VersionRef)
 */
class Predicate : public Step {
 public:
  class IsOK;
  class IsError;
  class MetadataMatch;
  class ContentsMatch;
};

/**
 * Require that a reference was successful (e.g. it did not return an error code)
 */
class Predicate::IsOK : public Predicate {
  // Default constructor for deserialization
  friend class cereal::access;
  IsOK() = default;

 public:
  /// Create an IS_OK predicate
  IsOK(shared_ptr<Reference> ref) : _ref(ref) {}

  /// Print an IS_OK predicate to an output stream
  virtual ostream& print(ostream& o) const { return o << "IS_OK(" << _ref->getName() << ")"; }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, IsOK& p);

 private:
  shared_ptr<Reference> _ref;  //< The reference that must have been made successfully
};

/**
 * Require that a reference resulted in a specific error code
 */
class Predicate::IsError : public Predicate {
  // Default constructor for deserialization
  friend class cereal::access;
  IsError() = default;

 public:
  /// Create an IS_ERROR predicate
  IsError(shared_ptr<Reference> ref, int err) : _ref(ref), _err(err) {}

  /// Print an IS_ERROR predicate
  virtual ostream& print(ostream& o) const {
    // Set up a map from error codes to names
    static map<int, string> errors = {{EACCES, "EACCES"}, {EDQUOT, "EDQUOT"}, {EEXIST, "EEXIST"},
                                      {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"}, {ELOOP, "ELOOP"},
                                      {ENOENT, "ENOENT"}};

    // If we can't identify the error code, just print "EMYSTERY"
    string errname = "EMYSTERY";

    // Look up the error name in our map
    auto iter = errors.find(-_err);
    if (iter != errors.end()) {
      errname = iter->second;
    }

    return o << "IS_ERROR(" << _ref->getName() << ", " << errname << ")";
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, IsError& p);

 private:
  shared_ptr<Reference> _ref;  //< The reference that must have resulted in an error
  int _err;                    //< The error code returned from the reference
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
  MetadataMatch(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected artifact version
  Artifact::VersionRef getVersion() const { return _version; }

  /// Print a METADATA_MATCH predicate
  virtual ostream& print(ostream& o) const {
    return o << "METADATA_MATCH(" << _ref->getName() << ", " << _version << ")";
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, MetadataMatch& p);

 private:
  shared_ptr<Reference> _ref;     //< The reference being examined
  Artifact::VersionRef _version;  //< The artifact version whose metadata the reference must match
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
  ContentsMatch(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this predicate
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the expected artifact version
  Artifact::VersionRef getVersion() const { return _version; }

  /// Print a CONTENTS_MATCH predicate
  virtual ostream& print(ostream& o) const {
    return o << "CONTENTS_MATCH(" << _ref->getName() << ", " << _version << ")";
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, ContentsMatch& p);

 private:
  shared_ptr<Reference> _ref;     //< The reference being examined
  Artifact::VersionRef _version;  //< The artifact version whose contents the reference must match
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

  /// Print a LAUNCH action
  virtual ostream& print(ostream& o) const;

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Launch& a);

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
  SetMetadata(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this action
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the artifact version that is put in place
  Artifact::VersionRef getVersion() const { return _version; }

  /// Print a SET_METADATA action
  virtual ostream& print(ostream& o) const {
    return o << "SET_METADATA(" << _ref->getName() << ", " << _version << ")";
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, SetMetadata& a);

 private:
  shared_ptr<Reference> _ref;     //< The reference used for this action
  Artifact::VersionRef _version;  //< The artifact version with the metadata written by this action
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
  SetContents(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Get the reference used for this action
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the artifact version that is put in place
  Artifact::VersionRef getVersion() const { return _version; }

  /// Print a SET_CONTENTS action
  virtual ostream& print(ostream& o) const {
    return o << "SET_CONTENTS(" << _ref->getName() << ", " << _version << ")";
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, SetContents& a);

 private:
  shared_ptr<Reference> _ref;     //< The reference used for this action
  Artifact::VersionRef _version;  //< The artifact version with the contents written by this action
};
