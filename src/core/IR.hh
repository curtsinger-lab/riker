#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>

#include <fcntl.h>

#include "core/Artifact.hh"

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

  /// Print this Step to an output stream
  virtual ostream& print(ostream& o) const = 0;

  /// Stream print wrapper for Step references
  friend ostream& operator<<(ostream& o, const Step& s) { return s.print(o); }

  /// Stream print wrapper for Step pointers
  friend ostream& operator<<(ostream& o, const Step* s) { return o << *s; }
};

/**
 * A Reference is created any time a command refers to an artifact. This happens when commands open
 * files, but other cases (like creating pipes) will also need to be tracked.
 *
 * The types of references are:
 *  - PIPE()
 * - ACCESS(<path>, <mode>)
 */
class Reference : public Step {
 public:
  class Pipe;
  class Access;

  /// Create a new reference
  Reference() : _id(getNextID()) {}

  /// Get the short name for this reference
  string getName() const { return "r" + std::to_string(_id); }

 private:
  size_t _id;

  /// Get a unique identifier for a reference
  static size_t getNextID() {
    static size_t next_id = 0;
    return next_id++;
  }
};

/// Create a reference to a new pipe
class Reference::Pipe : public Reference {
 public:
  virtual ostream& print(ostream& o) const { return o << getName() << " = PIPE()"; }
};

/// Access a filesystem path with a given set of flags
class Reference::Access : public Reference {
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

 private:
  string _path;
  Flags _flags;
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
 public:
  /// Create an IS_OK predicate
  IsOK(shared_ptr<Reference> ref) : _ref(ref) {}

  /// Print an IS_OK predicate to an output stream
  virtual ostream& print(ostream& o) const { return o << "IS_OK(" << _ref->getName() << ")"; }

 private:
  shared_ptr<Reference> _ref;
};

/**
 * Require that a reference resulted in a specific error code
 */
class Predicate::IsError : public Predicate {
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

 private:
  shared_ptr<Reference> _ref;
  int _err;
};

/**
 * Require that the metadata accessed through a reference matches that of an artifact version
 */
class Predicate::MetadataMatch : public Predicate {
 public:
  /// Create a METADATA_MATCH predicate
  MetadataMatch(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Print a METADATA_MATCH predicate
  virtual ostream& print(ostream& o) const {
    return o << "METADATA_MATCH(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Reference> _ref;
  Artifact::VersionRef _version;
};

/**
 * Require that the contents accessed through a reference match that of an artifact version
 */
class Predicate::ContentsMatch : public Predicate {
 public:
  /// Create a CONTENTS_MATCH predicate
  ContentsMatch(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Print a CONTENTS_MATCH predicate
  virtual ostream& print(ostream& o) const {
    return o << "CONTENTS_MATCH(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Reference> _ref;
  Artifact::VersionRef _version;
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
 public:
  /// Create a LAUNCH action
  Launch(shared_ptr<Command> cmd) : _cmd(cmd) {}

  /// Get the command this action launches
  shared_ptr<Command> getCommand() const { return _cmd; }

  /// Print a LAUNCH action
  virtual ostream& print(ostream& o) const;

 private:
  shared_ptr<Command> _cmd;
};

/**
 * A SetMetadata action indicates that a command set the metadata for an artifact.
 */
class Action::SetMetadata : public Action {
 public:
  /// Create a SET_METADATA action
  SetMetadata(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Print a SET_METADATA action
  virtual ostream& print(ostream& o) const {
    return o << "SET_METADATA(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Reference> _ref;
  Artifact::VersionRef _version;
};

/**
 * A SetContents action records that a command set the contents of an artifact.
 */
class Action::SetContents : public Action {
 public:
  /// Create a SET_CONTENTS action
  SetContents(shared_ptr<Reference> ref, Artifact::VersionRef version) :
      _ref(ref), _version(version) {}

  /// Print a SET_CONTENTS action
  virtual ostream& print(ostream& o) const {
    return o << "SET_CONTENTS(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Reference> _ref;
  Artifact::VersionRef _version;
};
