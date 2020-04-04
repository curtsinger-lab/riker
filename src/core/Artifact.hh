#pragma once

#include <cstdint>
#include <cstdio>
#include <initializer_list>
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>
#include <vector>

#include <fcntl.h>

#include "ui/options.hh"

class Command;

using std::enable_shared_from_this;
using std::list;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::vector;
using std::weak_ptr;

class Artifact : public enable_shared_from_this<Artifact> {
 public:
  // Forward declaration for Ref class, which stores a reference to an artifact.
  friend class Ref;

  // Forward declaration for VerionRef class, which stores a reference to a specific version of an
  // artifact.
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

  /// Get the unique ID assigned to this artifact
  size_t getId() const { return _id; }

  /// Get the path used to refer to this artifact
  const string& getPath() const { return _path; }

  /// Update the path used to refer to this artifact
  void updatePath(string path) { _path = path; }

  /// Get a short, printable name for this artifact
  string getShortName() const { return _path; }

  /// Get the type of this artifact
  Type getType() const { return _type; }

  /// Check if this artifact is only ever referenced
  bool onlyReferenced() const {
    return _versions.size() == 1 && _versions[0].action == Action::REFERENCE;
  }

  /// Check if this artifact corresponds to a system file
  bool isSystemFile() {
    for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
      // Check if the path begins with one of our prefixes.
      // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
      if (_path.rfind(p, 0) != string::npos) return true;
    }
    return false;
  }

  /// Get a string representation of this artifact's type
  string getTypeName() const {
    if (_type == Type::REGULAR) return "File";
    if (_type == Type::DIRECTORY) return "Directory";
    if (_type == Type::SYMLINK) return "Symlink";
    if (_type == Type::PIPE) return "Pipe";
    return "Artifact";
  }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& f) {
    if (f.getPath() != "")
      return o << "[" << f.getTypeName() << " " << f.getPath() << "]";
    else
      return o << "[" << f.getTypeName() << " " << f.getId() << "]";
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* f) { return o << *f; }

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

 public:
  const static shared_ptr<Artifact> stdin;
  const static shared_ptr<Artifact> stdout;
  const static shared_ptr<Artifact> stderr;
};
