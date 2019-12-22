#pragma once

#include <memory>
#include <optional>

#include <fcntl.h>

using std::make_shared;
using std::nullopt;
using std::optional;
using std::shared_ptr;

/// A reference to an artifact
class Ref : public std::enable_shared_from_this<Ref> {
  /// Only Commands can create Ref instances
  friend class Command;

 public:
  /// Flags that control the resolution of a reference
  struct Flags {
   public:
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
    static Flags fromStat(int flags) { return fromAccess(0, flags); }

    /// Create a Flags instance from the flags parameter to the chown syscall
    static Flags fromChown(int flags) { return fromAccess(0, flags); }

    /// Create a Flags instance from the flags parameter to the chmod syscall
    static Flags fromChmod(int flags) { return fromAccess(0, flags); }

    /// Create a Flags instance from the flags parameter to the unlink syscall
    static Flags fromUnlink(int flags) { return fromAccess(0, flags); }

    /// Print a Flags instance
    friend ostream& operator<<(ostream& o, const Flags& f) {
      o << (f.r ? 'r' : '-') << (f.w ? 'w' : '-') << (f.x ? 'x' : '-');
      if (f.nofollow) o << " nofollow";
      return o;
    }

    /// Is the reference readable?
    bool r = false;

    /// Is the reference writable?
    bool w = false;

    /// Is the reference executable?
    bool x = false;

    /// If the reference refers to a symlink, do we follow it?
    bool nofollow = false;

    /// Does the reference truncate the file when resolved?
    bool truncate = false;

    /// Does the reference create the file when resolved? May be as-needed, or required (see below)
    bool create = false;

    /// Is the reference *required* to create the file when resolved?
    bool exclusive = false;
  };

 private:
  /// Create a reference without a path
  Ref(shared_ptr<Command> cmd, Flags flags) : _command(cmd), _flags(flags), _anonymous(true) {}

  /// Create a reference with a path
  Ref(shared_ptr<Command> cmd, string path, Flags flags) :
      _command(cmd), _path(path), _flags(flags), _anonymous(false) {}

  /// Create an anonymous copy of this reference for use by a new command
  Ref(shared_ptr<Command> command, shared_ptr<Ref> other) :
      _command(command),
      _artifact(other->_artifact),
      _path(other->_path),
      _flags(other->_flags),
      _anonymous(true) {}

 public:
  // Disallow copy
  Ref(const Ref&) = delete;
  Ref& operator=(const Ref&) = delete;

  // Allow move
  Ref(Ref&&) = default;
  Ref& operator=(Ref&&) = default;

  /// Record the artifact this reference resolves to
  shared_ptr<Ref> resolvesTo(shared_ptr<Artifact> p) {
    _artifact = p;
    return shared_from_this();
  }

  /// Is this reference anonymous? True only if the command that made the reference did not use a
  /// path. This is the case for pipes, and for references inherited from parent commands.
  bool isAnonymous() const { return _anonymous; }

  /// Has this reference been resolved?
  bool isResolved() const { return _artifact.has_value(); }

  /// What artifact does this reference resolve to?
  shared_ptr<Artifact> getArtifact() const { return _artifact.value(); }

  /// Is this reference readable?
  bool isReadable() const { return _flags.r; }

  /// Is this reference writable?
  bool isWritable() const { return _flags.w; }

  /// Is this reference executable?
  bool isExecutable() const { return _flags.x; }

  /// Is this reference set up to create the artifact?
  bool canCreate() const { return _flags.create; }

  bool isExclusive() const { return _flags.exclusive; }

  /// Is this reference required to create the artifact?
  bool mustCreate() const { return canCreate() && isExclusive(); }

  /// Does this reference not follow links?
  bool isNoFollow() const { return _flags.nofollow; }

  /// Get the open() flags for this reference
  const Flags& getFlags() const { return _flags; }

  /// Does this reference have a path?
  bool hasPath() const { return _path.has_value(); }

  /// Get the path for this reference
  string getPath() const { return _path.value(); }

  /// Print this artifact reference
  friend ostream& operator<<(ostream& o, const Ref& ref) {
    o << ref._flags;
    if (ref._anonymous) o << " (anon)";
    if (ref.hasPath()) o << " " << ref.getPath();
    if (ref.isResolved()) o << " -> " << ref.getArtifact();
    return o;
  }

  // Print a Ref*
  friend ostream& operator<<(ostream& o, const Ref* ref) { return o << *ref; }

 private:
  /// The command that made this reference
  weak_ptr<Command> _command;

  /// The artifact this reference resolves to, if it has been resolved.
  optional<shared_ptr<Artifact>> _artifact;
  
  /// The path to the artifact. This will be set for any artifact that has a path, even if it is an
  /// anonymous reference (e.g. a file opened before an exec call).
  optional<string> _path;

  /// Flags that control the resolution of this reference
  Flags _flags;

  /// Is this reference made without naming the path to the artifact?
  /// This will be true for pipes, and for reference inherited from parent commands.
  bool _anonymous;
};
