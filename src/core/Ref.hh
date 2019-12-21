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
 private:
  /// Create an anonymous copy of this reference for use by a new command
  Ref(shared_ptr<Command> command, const Ref* other) :
      _command(command),
      _artifact(other->_artifact),
      _path(other->_path),
      _flags(other->_flags),
      _executable(other->_executable),
      _anonymous(true) {}

 public:
  /// Create a reference without a path
  Ref(shared_ptr<Command> command, int flags, bool executable) :
      _command(command), _flags(flags), _executable(executable), _anonymous(true) {}

  /// Create a reference with a path
  Ref(shared_ptr<Command> command, string path, int flags, bool executable) :
      _command(command), _path(path), _flags(flags), _executable(executable), _anonymous(false) {}

  // Disallow copy
  Ref& operator=(const Ref&) = delete;

  // Allow move
  Ref(Ref&&) = default;
  Ref& operator=(Ref&&) = default;

  /// Record the artifact this reference resolves to
  shared_ptr<Ref> resolvesTo(shared_ptr<Artifact> p) {
    _artifact = p;
    return shared_from_this();
  }

  /// Is this reference inherited by child commands?
  bool isInherited() const { return (_flags & O_CLOEXEC) == 0; }

  /// Is this reference anonymous? True only if the command that made the reference did not use a
  /// path. This is the case for pipes, and for references inherited from parent commands.
  bool isAnonymous() const { return _anonymous; }

  /// Get the anonymous, inherited version of this reference. This is used by commands that
  /// reference an artifact that was opened by the parent command prior to calling exec.
  shared_ptr<Ref> getInheritedRef(shared_ptr<Command> command) const {
    return shared_ptr<Ref>(new Ref(command, this));
  }

  /// Has this reference been resolved?
  bool isResolved() const { return _artifact.has_value(); }

  /// What artifact does this reference resolve to?
  shared_ptr<Artifact> getArtifact() const { return _artifact.value(); }

  /// Is this reference readable?
  bool isReadable() const { return (_flags & O_RDONLY) == O_RDONLY || (_flags & O_RDWR) == O_RDWR; }

  /// Is this reference writable?
  bool isWritable() const { return (_flags & O_WRONLY) == O_WRONLY || (_flags & O_RDWR) == O_RDWR; }

  /// Is this reference executable?
  bool isExecutable() const { return _executable; }

  /// Is this reference closed on exec?
  bool isCloexec() const { return (_flags & O_CLOEXEC) == O_CLOEXEC; }

  /// Set the reference's cloexec status
  void setCloexec(bool c) {
    if (c)
      _flags |= O_CLOEXEC;
    else
      _flags &= ~O_CLOEXEC;
  }

  /// Is this reference set up to create the artifact?
  bool canCreate() const { return (_flags & O_CREAT) == O_CREAT; }

  bool isExclusive() const { return (_flags & O_EXCL) == O_EXCL; }

  /// Is this reference required to create the artifact?
  bool mustCreate() const { return canCreate() && isExclusive(); }

  /// Does this reference not follow links?
  bool isNoFollow() const { return (_flags & O_NOFOLLOW) == O_NOFOLLOW; }

  /// Get the open() flags for this reference
  int getFlags() const { return _flags; }

  /// Does this reference have a path?
  bool hasPath() const { return _path.has_value(); }

  /// Get the path for this reference
  string getPath() const { return _path.value(); }

  /// Print this artifact reference
  friend ostream& operator<<(ostream& o, const Ref& ref) {
    string p = ref.hasPath() ? ref.getPath() + " " : "";
    string r = ref.isReadable() ? "r" : "-";
    string w = ref.isWritable() ? "w" : "-";
    string x = ref.isExecutable() ? "x" : "-";
    string c = ref.isCloexec() ? " cloexec" : "";

    return o << p << r << w << x << c << " -> " << ref.getArtifact();
  }

 private:
  /// The command that made this reference
  weak_ptr<Command> _command;

  /// The artifact this reference resolves to. This is optional because some references are
  /// path-only, and no attempt is made to resolve them. If there is an artifact pointer but it is
  /// not set, that means the reference failed to resolve.
  optional<shared_ptr<Artifact>> _artifact = nullopt;

  /// The path used to reach the artifact. If not set, the reference was not established via path.
  /// This will happen for pipes, but we should know the path for any entity on the filesystem.
  /// That's true even when a command establishes a reference through a sequence of accesses like
  /// openat() or fchdir(). The file descriptors those calls use to reference locations are known
  /// and their paths can be accessed and composed to generate full paths for any reference.
  optional<string> _path;

  /// Flags from the open syscall. This captures a few things we care about:
  ///  is the reference readable?
  ///  is the reference writable?
  ///  is the reference closed on an exec call? (O_CLOEXEC)
  ///  is the reference set up to create the file? (O_CREAT)
  ///  is the reference required to create the file? (O_EXCL)
  ///  is the reference opened without following links? (O_NOFOLLOW)
  /// We need to track these flags because they allow us to determine whether this reference will
  /// resolve differently on some future run of the command that makes the reference.
  int _flags;

  /// Is the reference made in a way that requires execute permissions?
  bool _executable;

  /// Is this reference made without naming the path to the artifact?
  /// This will be true for pipes, and for reference inherited from parent commands.
  bool _anonymous;
};
