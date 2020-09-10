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
    return s->print(o);
  }

 private:
  SERIALIZE_EMPTY();
};

/***
 * A RefResult instance is a bit like a register; it is serialized as the destination where a
 * resolved reference will be saved. An IR Step that resolves a reference will also have a RefResult
 * pointer where it will store the resolution result. When serialized, RefResults hold no data; the
 * only important aspect of a RefResult in the serialized trace is its identity. One step will
 * resolve a reference and save the outcome in a RefResult, and later steps may reference that
 * RefResult to modify or compare contents of the resolved artifact.
 */
class RefResult final {
 public:
  /// Default constructor
  RefResult() noexcept = default;

  // Disallow Copy
  RefResult(const RefResult&) = delete;
  RefResult& operator=(const RefResult&) = delete;

  // Allow Move
  RefResult(RefResult&&) noexcept = default;
  RefResult& operator=(RefResult&&) noexcept = default;

  /// Get this RefResult's unique ID
  size_t getID() const noexcept { return _id; }

  /// Get a short name for this RefResult
  string getName() const noexcept { return "r" + std::to_string(getID()); }

  /// Get the resolution result
  Resolution getResult() const noexcept { return _result; }

  /// Save a resolution result in this RefResult
  void resolvesTo(Resolution r) noexcept { _result = r; }

  /// Print a RefResult
  ostream& print(ostream& o) const noexcept { return o << getName(); }

  /// Stream print wrapper for RefResult references
  friend ostream& operator<<(ostream& o, const RefResult& r) noexcept { return r.print(o); }

  /// Stream print wrapper for RefResult pointers
  friend ostream& operator<<(ostream& o, const RefResult* r) noexcept {
    if (r == nullptr) return o << "<null RefResult>";
    return o << *r;
  }

 private:
  /// RefResults are serialized, but we only track their identity. All other fields are transient.
  SERIALIZE_EMPTY();

  /// A unique identifier for this reference result
  UniqueID<RefResult> _id;

  /// The outcome of a reference resolution saved in this RefResult
  Resolution _result;
};

/**
 * Resolve a reference to a special artifact like stdin, stdout, the root directory, etc.
 */
class SpecialRef final : public Step {
 public:
  enum Entity { stdin, stdout, stderr, root, cwd, launch_exe };

  /// Create a new special reference
  SpecialRef(Entity entity, shared_ptr<RefResult> output) noexcept :
      _entity(entity), _output(output) {}

  /// Get the entity this special reference refers to
  Entity getEntity() const noexcept { return _entity; }

  /// Get the name of the entity this special reference refers to
  string getEntityName() const noexcept {
    if (_entity == stdin) return "STDIN";
    if (_entity == stdout) return "STDOUT";
    if (_entity == stderr) return "STDERR";
    if (_entity == root) return "ROOT";
    if (_entity == cwd) return "CWD";
    if (_entity == launch_exe) return "LAUNCH_EXE";
    return "UNKNOWN";
  }

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a SpecialRef step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << _output << " = " << getEntityName();
  }

 private:
  /// The special artifact this reference refers to
  Entity _entity;

  /// The artifact or error code produced by resolving this reference is stored in this RefResult
  shared_ptr<RefResult> _output;

  // Create a default constructor and declare fields for serialization
  SpecialRef() noexcept = default;
  SERIALIZE(BASE(Step), _entity, _output);
};

/// Create a reference to a new anonymous pipe
class PipeRef final : public Step {
 public:
  /// Create a reference to an anonymous pipe
  PipeRef(shared_ptr<RefResult> read_end, shared_ptr<RefResult> write_end) noexcept :
      _read_end(read_end), _write_end(write_end) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a PipeRef step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[" << _read_end << ", " << _write_end << "] = PipeRef()";
  }

 private:
  shared_ptr<RefResult> _read_end;   //< A reference to the read end of the pipe
  shared_ptr<RefResult> _write_end;  //< A reference to the write end of the pipe

  // Create a default constructor and declare fields for serialization
  PipeRef() noexcept = default;
  SERIALIZE(BASE(Step), _read_end, _write_end);
};

/// Create a reference to a new anonymous file
class FileRef final : public Step {
 public:
  /// Create a reference to an anonymous file
  FileRef(mode_t mode, shared_ptr<RefResult> output) noexcept : _mode(mode), _output(output) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a FileRef step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << _output << " = FileRef(" << std::oct << _mode << ")";
  }

 private:
  /// The value that should be passed in the open() syscall's mode parameter
  mode_t _mode;

  /// The artifact or error code produced by resolving this reference is stored in this RefResult
  shared_ptr<RefResult> _output;

  // Create a default constructor and declare fields for serialization
  FileRef() = default;
  SERIALIZE(BASE(Step), _mode, _output);
};

/// Create a reference to a new anonymous symlink
class SymlinkRef final : public Step {
 public:
  // Create a reference to an anonymous symlink
  SymlinkRef(fs::path target, shared_ptr<RefResult> output) noexcept :
      _target(target), _output(output) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a SYMLINK reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << _output << " = SymlinkRef(" << _target << ")";
  }

 private:
  /// The target path for the new symlink
  fs::path _target;

  /// The artifact or error code produced by resolving this reference is stored in this RefResult
  shared_ptr<RefResult> _output;

  // Create a default constructor and specify fields for serialization
  SymlinkRef() = default;
  SERIALIZE(BASE(Step), _target, _output);
};

/// Create a reference to a new anonymous directory
class DirRef final : public Step {
 public:
  /// Create a reference to an anonymous directory
  DirRef(mode_t mode, shared_ptr<RefResult> output) noexcept : _mode(mode), _output(output) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a DIR reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << _output << " = DirRef(" << std::oct << _mode << ")";
  }

 private:
  /// The value that should be passed as the open() syscall's mode parameter
  mode_t _mode;

  /// The artifact or error code produced by resolving this reference is stored in this RefResult
  shared_ptr<RefResult> _output;

  // Create a default constructor and specify fields for serialization
  DirRef() = default;
  SERIALIZE(BASE(Step), _mode, _output);
};

/// Make a reference to a filesystem path
class PathRef final : public Step {
 public:
  /// Create a reference to a filesystem path
  PathRef(shared_ptr<RefResult> base,
          fs::path path,
          AccessFlags flags,
          shared_ptr<RefResult> output) noexcept :
      _base(base), _path(path), _flags(flags), _output(output) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Get the access that serves as the base for this one
  const shared_ptr<RefResult>& getBase() const noexcept { return _base; }

  /// Get the path of this reference, relative to the base access
  fs::path getRelativePath() const noexcept { return _path; }

  /// Get the flags used to create this reference
  const AccessFlags& getFlags() const noexcept { return _flags; }

  /// Print an ACCESS reference
  virtual ostream& print(ostream& o) const noexcept override {
    return o << _output << " = PathRef(" << _base << ", " << _path << ", [" << getFlags() << "])";
  }

 private:
  /// The base used to resolve this reference, usually either cwd or root.
  shared_ptr<RefResult> _base;

  /// The path being accessed
  fs::path _path;

  /// The relevant flags for the access
  AccessFlags _flags;

  /// The artifact or error code produced by resolving this reference is stored in this RefResult
  shared_ptr<RefResult> _output;

  // Create default constructor and specify fields for serialization
  PathRef() = default;
  SERIALIZE(BASE(Step), _base, _path, _flags, _output);
};

/**
 * A command expects a reference to resolve a particular way
 */
class ExpectResult final : public Step {
 public:
  /// Create an ExpectResult IR step
  ExpectResult(shared_ptr<RefResult> ref, int expected) noexcept : _ref(ref), _expected(expected) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an ExpectResult IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "ExpectResult(" << _ref << ", " << errors[_expected] << ")";
  }

 private:
  /// The result of some reference resolution
  shared_ptr<RefResult> _ref;

  /// The expected outcome of the reference resolution
  int _expected;

  // Create default constructor and specify fields for serialization
  ExpectResult() noexcept = default;
  SERIALIZE(BASE(Step), _ref, _expected);
};

/**
 * A command expects to find specific metadata in an artifact reached via reference
 */
class MatchMetadata final : public Step {
 public:
  /// Create a MatchMetadata IR step
  MatchMetadata(shared_ptr<RefResult> ref, shared_ptr<MetadataVersion> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a MatchMetadata IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "MatchMetadata(" << _ref << ", " << _version << ")";
  }

 private:
  shared_ptr<RefResult> _ref;            //< A resolved reference to the artifact being accessed
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
  /// Create a MatchContent IR step
  MatchContent(shared_ptr<RefResult> ref, shared_ptr<Version> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a MatchContent IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "MatchContent(" << _ref << ", " << _version << ")";
  }

 private:
  shared_ptr<RefResult> _ref;    //< A resolved reference to the artifact being accessed
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
  UpdateMetadata(shared_ptr<RefResult> ref, shared_ptr<MetadataVersion> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an UpdateMetadata IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "UpdateMetadata(" << _ref << ", " << _version << ")";
  }

 private:
  shared_ptr<RefResult> _ref;            //< A resolved reference to the artifact being written
  shared_ptr<MetadataVersion> _version;  //< The version written to the referenced artifact

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
  UpdateContent(shared_ptr<RefResult> ref, shared_ptr<Version> version) noexcept :
      _ref(ref), _version(version) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an UpdateContent IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "UpdateContent(" << _ref << ", " << _version << ")";
  }

 private:
  shared_ptr<RefResult> _ref;    //< A resolved reference to the artifact being written
  shared_ptr<Version> _version;  //< The version written to the referenced artifact

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
  /// Create a Launch IR step
  Launch(shared_ptr<Command> cmd) noexcept : _cmd(cmd) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a Launch IR step
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
  /// Create a Join IR step
  Join(shared_ptr<Command> cmd, int exit_status) noexcept : _cmd(cmd), _exit_status(exit_status) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print a JOin IR step
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
  /// Create an Exit IR step
  Exit(int exit_status) noexcept : _exit_status(exit_status) {}

  /// Emulate this step in the context of a given build
  virtual void emulate(shared_ptr<Command> c, Build& build) noexcept override;

  /// Print an Exit IR step
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "Exit(" << _exit_status << ")";
  }

 private:
  int _exit_status;

  // Create default constructor and specify fields for serialization
  Exit() = default;
  SERIALIZE(BASE(Step), _exit_status);
};
