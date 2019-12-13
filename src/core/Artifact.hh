#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iosfwd>
#include <list>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "ui/log.hh"

class Command;

using std::enable_shared_from_this;
using std::list;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::vector;
using std::weak_ptr;

class Artifact : public enable_shared_from_this<Artifact> {
 public:
  // Forward declaration for Ref class, which stores a reference to an artifact.
  class Ref;
  friend class Ref;

  // Forward declaration for VerionRef class, which stores a reference to a specific version of an
  // artifact.
  class VersionRef;
  friend class VersionRef;

  /// Types of artifacts we track
  enum class Type { UNKNOWN, REGULAR, DIRECTORY, SYMLINK, PIPE };

  /// Actions that can create new versions of an artifact
  enum class Action { CREATE, REFERENCE, WRITE, TRUNCATE, DELETE };

  /****** Constructors ******/
  Artifact(string path, Type type = Type::UNKNOWN) : _id(next_id++), _path(path), _type(type) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /****** Non-trivial methods ******/

  /// Called during or after a command's creation of this artifact
  void createdBy(shared_ptr<Command> c);

  /// Called during or after a command's read from this artifact
  void readBy(shared_ptr<Command> c);

  /// Called just before allowing command c to write this artifact. May cache or fingerprint.
  void mayWrite(shared_ptr<Command> c);

  /// Called after a command writes to this artifact
  void writtenBy(shared_ptr<Command> c);

  /// Called just before allowing command c to truncate this artifact. May cache or fingerprint.
  void mayTruncate(shared_ptr<Command> c);

  /// Called after a command truncates this artifact to zero length
  void truncatedBy(shared_ptr<Command> c);

  /// Called just before allowing command c to delete this artifact. May cache or fingerprint.
  void mayDelete(shared_ptr<Command> c);

  /// Called after a command unlinks this artifact
  void deletedBy(shared_ptr<Command> c);

  /// Called after a command mmaps this artifact
  void mappedBy(shared_ptr<Command> c, bool writable);

  /// Called after a command unmaps this artifact (partial unmaps should not call this function)
  void unmappedBy(shared_ptr<Command> c, bool writable);

  /****** Getters and setters ******/

  size_t getId() const { return _id; }

  const string& getPath() const { return _path; }

  void updatePath(string path) { _path = path; }

  string getShortName() const { return _path; }

  Type getType() const { return _type; }

  bool isLocal() const { return _path[0] != '/'; }

  bool isSystemFile() {
    for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
      // Check if the path begins with one of our prefixes.
      // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
      if (_path.rfind(p, 0) != string::npos) return true;
    }
    return false;
  }
  
  string getTypeName() const {
    if (_type == Type::REGULAR) return "File";
    if (_type == Type::DIRECTORY) return "Directory";
    if (_type == Type::SYMLINK) return "Symlink";
    if (_type == Type::PIPE) return "Pipe";
    return "Artifact";
  }
  
  friend ostream& operator<<(ostream& o, const Artifact& f) {
    return o << "[" << f.getTypeName() << " " << f.getPath() << "]";
  }
  
  friend ostream& operator<<(ostream& o, const Artifact* f) {
    return o << *f;
  }

  /// A reference to this artifact
  class Ref {
    friend class Artifact;

   public:
    /// Create a reference without a path
    Ref(shared_ptr<Artifact> artifact, int flags, bool executable) :
        _artifact(artifact), _flags(flags), _executable(executable) {}

    /// Create a reference with a path
    Ref(shared_ptr<Artifact> artifact, int flags, bool executable, string path) :
        _artifact(artifact), _flags(flags), _executable(executable), _path(path) {}

    /// What artifact does this reference resolve to?
    shared_ptr<Artifact> getArtifact() const { return _artifact; }

    /// Is this reference readable?
    bool isReadable() const {
      return (_flags & O_RDONLY) == O_RDONLY || (_flags & O_RDWR) == O_RDWR;
    }

    /// Is this reference writable?
    bool isWritable() const {
      return (_flags & O_WRONLY) == O_WRONLY || (_flags & O_RDWR) == O_RDWR;
    }

    /// Is this reference executable?
    bool isExecutable() const { return _executable; }

    /// Is this reference closed on exec?
    bool isCloexec() const {
      return (_flags & O_CLOEXEC) == O_CLOEXEC;
    }
    
    /// Set the reference's cloexec status
    void setCloexec(bool c) {
      if (c) _flags |= O_CLOEXEC;
      else _flags &= ~O_CLOEXEC;
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
    
    friend ostream& operator<<(ostream& o, const Artifact::Ref& ref) {
      string p = ref.hasPath() ? ref.getPath() + " " : "";
      string r = ref.isReadable() ? "r" : "-";
      string w = ref.isWritable() ? "w" : "-";
      string x = ref.isExecutable() ? "x" : "-";
      string c = ref.isCloexec() ? " cloexec" : "";
  
      return o << p << r << w << x << c << " -> " << ref.getArtifact();
    }

   private:
    /// The actual artifact reached through this reference. If unset, the reference did not
    /// resolve.
    shared_ptr<Artifact> _artifact;

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

    /// The path used to reach the artifact. If not set, the reference was not established via path.
    /// This will happen for pipes, but we should know the path for any entity on the filesystem.
    /// That's true even when a command establishes a reference through a sequence of accesses like
    /// openat() or fchdir(). The file descriptors those calls use to reference locations are known
    /// and their paths can be accessed and composed to generate full paths for any reference.
    optional<string> _path;
  };

  /// A reference to a specific version of this artifact
  class VersionRef {
    friend class Artifact;

   private:
    VersionRef(shared_ptr<Artifact> artifact, size_t index) : _artifact(artifact), _index(index) {}

   public:
    shared_ptr<Artifact> getArtifact() const { return _artifact; }
    size_t getIndex() const { return _index; }
    Action getAction() const { return _artifact->_versions[_index].action; }
    shared_ptr<Command> getWriter() const { return _artifact->_versions[_index].writer.lock(); }
    string getShortName() const { return _artifact->getShortName() + "v" + to_string(_index); }

    string getActionName() const {
      switch (getAction()) {
        case Action::CREATE:
          return "create";
        case Artifact::Action::REFERENCE:
          return "ref";
        case Artifact::Action::WRITE:
          return "write";
        case Artifact::Action::TRUNCATE:
          return "truncate";
        case Artifact::Action::DELETE:
          return "delete";
      }
    }

    bool operator<(const VersionRef& other) const {
      return std::tie(_artifact, _index) < std::tie(other._artifact, other._index);
    }

   private:
    shared_ptr<Artifact> _artifact;
    size_t _index;
  };

  /// Get a reference to the latest version of this artifact
  VersionRef getLatestVersion() { return VersionRef(shared_from_this(), _versions.size() - 1); }

  /// Construct a list of references to the versions of this artifact. This isn't particularly
  /// efficient, but it's only used in the GraphViz output.
  const list<VersionRef> getVersions() {
    list<VersionRef> result;
    for (size_t i = 0; i < _versions.size(); i++) {
      result.push_back(VersionRef(shared_from_this(), i));
    }
    return result;
  }
  
  friend ostream& operator<<(ostream& o, const Artifact::VersionRef& v) {
    return o << v.getArtifact() << "@" << v.getIndex();
  }

 private:
  /// Data about a specific version of this artifact. This struct is hidden from outside users.
  /// Outside code should use Artifact::VersionRef to refer to a specific version of an artifact.
  struct Version {
    Version(Action action) : action(action) {}
    Version(Action action, shared_ptr<Command> writer) : action(action), writer(writer) {}

    Action action;             //< The action that created this version
    weak_ptr<Command> writer;  //< The command that created this artifact version

    bool has_metadata = false;  //< Do we have file metadata for this version?
    struct stat metadata;       //< If we have it, the metadata is stored here

    bool has_fingerprint = false;  //< Do we have a fingerprint of the file contents?
    vector<uint8_t> fingerprint;   //< The fingerprint
  };

  /// Tag a new version of this artifact and return a reference to that version
  VersionRef makeVersion(Action action, shared_ptr<Command> writer = nullptr);

  /// Fingerprint this artifact and save the fingerprint with the latest version of the artifact
  void fingerprint();

 private:
  size_t _id;
  string _path;                                //< The absolute, normalized path to this artifact
  Type _type = Type::UNKNOWN;                  //< The type of artifact being tracked
  vector<Version> _versions;                   //< The sequence of versions of this artifact
  list<weak_ptr<Command>> _writable_mappers;   //< Commands that map this artifact writable
  list<weak_ptr<Command>> _read_only_mappers;  //< Commands that map this artifact read-only

  static size_t next_id;
};
