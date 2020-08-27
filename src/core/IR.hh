#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>

#include <sys/stat.h>
#include <sys/types.h>

#include "artifacts/Artifact.hh"
#include "build/Resolution.hh"
#include "core/AccessFlags.hh"
#include "util/UniqueID.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::map;
using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

class Artifact;
class Build;
class Command;
class Env;
class FileVersion;
class MetadataVersion;
class Resolve;
class SymlinkVersion;
class Version;

/**
 * A Command's actions are tracked as a sequence of Steps, each corresponding to some operation
 * or dependency we observed the last time a command executed.
 *
 * Step is the abstract parent class for all IR values.
 * All command steps fall into one of three categories:
 * - Ref: a reference to some artifact made by a command
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
  friend ostream& operator<<(ostream& o, const Step* s) noexcept {
    if (s == nullptr) return o << "<null Step>";
    return o << *s;
  }

 private:
  SERIALIZE_EMPTY();
};

/**
 * Any time a command makes a reference to an artifact we will record it with an IR step that is a
 * subclass of Ref. Refs do not necessarily resolve to artifacts (they could fail) but
 * we can encode predicates about the outcome of a reference.
 */
class Ref : public std::enable_shared_from_this<Ref> {
 public:
  /// Get the path associated with this reference, if any
  virtual optional<fs::path> getPath() const noexcept { return nullopt; }

  /// Resolve this reference
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept = 0;

  /// Print this Ref to an output stream
  virtual ostream& print(ostream& o) const noexcept = 0;

  /// Stream print wrapper for Ref references
  friend ostream& operator<<(ostream& o, const Ref& r) noexcept { return r.print(o); }

  /// Stream print wrapper for Ref pointers
  friend ostream& operator<<(ostream& o, const Ref* r) noexcept {
    if (r == nullptr) return o << "<null Ref>";
    return o << *r;
  }

 private:
  SERIALIZE_EMPTY();
};

class Resolve final : public Step {
 public:
  Resolve(shared_ptr<Ref> ref) noexcept : _ref(ref) {}

  /// Get the reference this step resolves
  shared_ptr<Ref> getRef() const noexcept { return _ref; }

  /// Get the unique ID for this reference
  size_t getID() const noexcept { return _id; }

  /// Get the short name for this reference
  string getName() const noexcept { return "r" + std::to_string(getID()); }

  /// Get the result of resolving this reference
  Resolution getResolution() const noexcept { return _res; }

  /// Check if this reference is resolved
  bool isResolved() const noexcept { return _res; }

  /// Get the artifact this reference resolved to
  shared_ptr<Artifact> getArtifact() const noexcept { return _res; }

  /// A sub-type can report the result of resolving this artifact using this method
  void resolvesTo(Resolution res) noexcept { _res = res; }

  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  virtual ostream& print(ostream& o) const noexcept override {
    return o << getName() << " = Resolve(" << _ref << ")";
  }

 private:
  shared_ptr<Ref> _ref;

  // Create default constructor and specify fields for serialization
  Resolve() noexcept = default;
  SERIALIZE(BASE(Step), _ref);

  /****** Transient Fields ******/

  /// Assign a unique ID to each reference
  UniqueID<Resolve> _id;

  /// The result of resolving this reference
  Resolution _res;
};

/**
 * A command expects a reference to resolve a particular way
 */
class ExpectResult final : public Step {
 public:
  /// Create an ExpectResult predicate
  ExpectResult(shared_ptr<Resolve> ref, int expected) noexcept : _ref(ref), _expected(expected) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  virtual ostream& print(ostream& o) const noexcept override {
    return o << "ExpectResult(" << _ref->getName() << ", " << errors[_expected] << ")";
  }

 private:
  shared_ptr<Resolve> _ref;  //< The result of resolving a reference
  int _expected;             //< The expected result of resolving the reference

  // Create default constructor and specify fields for serialization
  ExpectResult() noexcept = default;
  SERIALIZE(BASE(Step), _ref, _expected);
};

/**
 * A command expects to find specific metadata in an artifact reached via reference
 */
class MatchMetadata final : public Step {
 public:
  /// Create a MatchMetadata predicate
  MatchMetadata(shared_ptr<Resolve> ref, shared_ptr<MetadataVersion> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a MATCH predicate
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "MatchMetadata(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Resolve> _ref;              //< The reference being examined
  shared_ptr<MetadataVersion> _version;  //< The expected metadata

  // Create default constructor and specify fields for serialization
  MatchMetadata() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * A command expects to find specific contents in an artifact reached via reference
 */
class MatchContent final : public Step {
 public:
  /// Create a MatchContent predicate
  MatchContent(shared_ptr<Resolve> ref, shared_ptr<Version> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a MATCH predicate
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "MatchContent(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Resolve> _ref;      //< The reference being examined
  shared_ptr<Version> _version;  //< The expected content

  // Create default constructor and specify fields for serialization
  MatchContent() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * A command writes a metadata version through a reference
 */
class UpdateMetadata final : public Step {
 public:
  /// Create an UpdateMetadata IR step
  UpdateMetadata(shared_ptr<Resolve> ref, shared_ptr<MetadataVersion> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an UpdateMetadata IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "UpdateMetadata(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Resolve> _ref;
  shared_ptr<MetadataVersion> _version;

  // Create default constructor and specify fields for serialization
  UpdateMetadata() = default;
  SERIALIZE(BASE(Step), _ref, _version);
};

/**
 * A command writes a content version through a reference
 */
class UpdateContent final : public Step {
 public:
  /// Create an UpdateContent IR step
  UpdateContent(shared_ptr<Resolve> ref, shared_ptr<Version> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an UpdateContent IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "UpdateContent(" << _ref->getName() << ", " << _version << ")";
  }

 private:
  shared_ptr<Resolve> _ref;
  shared_ptr<Version> _version;

  // Create default constructor and specify fields for serialization
  UpdateContent() = default;
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
    return o << "Launch(" << _cmd << ")";
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
    return o << "Join(" << _cmd << ", " << _exit_status << ")";
  }

 private:
  shared_ptr<Command> _cmd;  //< The command that was joined with
  int _exit_status;          //< The exit status of the child

  // Create default constructor and specify fields for serialization
  Join() = default;
  SERIALIZE(BASE(Step), _cmd, _exit_status);
};

class Exit final : public Step {
 public:
  /// Create an EXIT action
  Exit(int exit_status) noexcept : _exit_status(exit_status) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an EXIT action
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "Exit(" << _exit_status << ")";
  }

 private:
  int _exit_status;

  // Create default constructor and specify fields for serialization
  Exit() = default;
  SERIALIZE(BASE(Step), _exit_status);
};

/// Create a reference to a special artifact (standard pipes, root directory, etc.)
class SpecialRef final : public Ref {
 public:
  enum Entity { stdin, stdout, stderr, root, cwd, launch_exe };

  /// Create a new special reference
  SpecialRef(Entity entity) noexcept : _entity(entity) {}

  /// Get the entity this special reference refers to
  Entity getEntity() const noexcept { return _entity; }

  /// Resolve this reference in the context of a given build
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept override;

  /// Get the path associated with this reference
  virtual optional<fs::path> getPath() const noexcept override {
    if (_entity == root) return "/";
    return nullopt;
  }

  /// Print a special reference
  virtual ostream& print(ostream& o) const noexcept override {
    switch (_entity) {
      case stdin:
        return o << "stdin";

      case stdout:
        return o << "stdout";

      case stderr:
        return o << "stderr";

      case root:
        return o << "root";

      case cwd:
        return o << "cwd";

      case launch_exe:
        return o << "launch_exe";
    }
  }

 private:
  Entity _entity;

  // Create a default constructor and declare fields for serialization
  SpecialRef() noexcept = default;
  SERIALIZE(BASE(Ref), _entity);
};

/// Create a reference to a new anonymous pipe
class PipeRef final : public Ref {
 public:
  /// Create a reference to an anonymous pipe
  PipeRef() noexcept = default;

  /// Resolve this reference in the context of a given build
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept override;

  /// Print a PIPE reference
  virtual ostream& print(ostream& o) const noexcept override { return o << "PipeRef()"; }

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(Ref));
};

/// Create a reference to a new anonymous file
class FileRef final : public Ref {
 public:
  /// Create a reference to an anonymous file
  FileRef(mode_t mode) noexcept : _mode(mode) {}

  /// Resolve this reference in the context of a given build
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept override;

  /// Print a FILE reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "FileRef(" << std::oct << _mode << ")";
  }

 private:
  mode_t _mode;

  // Specify fields for serialization
  FileRef() = default;
  SERIALIZE(BASE(Ref), _mode);
};

/// Create a reference to a new anonymous symlink
class SymlinkRef final : public Ref {
 public:
  // Create a reference to an anonymous symlink
  SymlinkRef(fs::path target) noexcept : _target(target) {}

  /// Resolve this reference in the context of a given build
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept override;

  /// Print a SYMLINK reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "SymlinkRef(" << _target << ")";
  }

 private:
  fs::path _target;

  // Specify fields for serialization
  SymlinkRef() = default;
  SERIALIZE(BASE(Ref), _target);
};

/// Create a reference to a new anonymous directory
class DirRef final : public Ref {
 public:
  /// Create a reference to an anonymous directory
  DirRef(mode_t mode) noexcept : _mode(mode) {}

  /// Resolve this reference in the context of a given build
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept override;

  /// Print a DIR reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "DirRef(" << std::oct << _mode << ")";
  }

 private:
  mode_t _mode;

  // Specify fields for serialization
  DirRef() = default;
  SERIALIZE(BASE(Ref), _mode);
};

/// Make a reference to a filesystem path
class PathRef final : public Ref {
 public:
  /// Create a reference to a filesystem path
  PathRef(shared_ptr<Resolve> base, fs::path path, AccessFlags flags) noexcept :
      _base(base), _path(path), _flags(flags) {}

  /// Resolve this reference in the context of a given build
  virtual Resolution resolve(shared_ptr<Command> c,
                             Build& build,
                             shared_ptr<Resolve> result,
                             bool committed) noexcept override;

  /// Get the access that serves as the base for this one
  const shared_ptr<Resolve>& getBase() const noexcept { return _base; }

  /// Get the path of this reference, relative to the base access
  fs::path getRelativePath() const noexcept { return _path; }

  /// Get the full path for this reference
  virtual optional<fs::path> getPath() const noexcept override {
    if (!_base) return _path;

    auto base_path = _base->getArtifact()->getPath();
    if (!base_path.has_value()) return nullopt;
    return base_path.value() / _path;
  }

  /// Get the flags used to create this reference
  const AccessFlags& getFlags() const noexcept { return _flags; }

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "PathRef(" << _base->getName() << ", " << _path << ", [" << getFlags() << "])";
  }

 private:
  /// The base used to resolve this reference, typically either cwd or root.
  shared_ptr<Resolve> _base;

  /// The path being accessed
  fs::path _path;

  /// The relevant flags for the access
  AccessFlags _flags;

  // Create default constructor and specify fields for serialization
  PathRef() = default;
  SERIALIZE(BASE(Ref), _base, _path, _flags);
};
