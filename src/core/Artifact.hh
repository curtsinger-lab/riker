#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iosfwd>
#include <list>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <sys/stat.h>

class Command;

using std::enable_shared_from_this;
using std::list;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::vector;
using std::weak_ptr;

class Artifact : public enable_shared_from_this<Artifact> {
 public:
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

 private:
  /// Data about a specific version of this artifact
  struct VersionRecord {
    VersionRecord(Action action) : action(action) {}
    VersionRecord(Action action, shared_ptr<Command> writer) : action(action), writer(writer) {}

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

 public:
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

 private:
  size_t _id;
  string _path;                                //< The absolute, normalized path to this artifact
  Type _type = Type::UNKNOWN;                  //< The type of artifact being tracked
  vector<VersionRecord> _versions;             //< The sequence of versions of this artifact
  list<weak_ptr<Command>> _writable_mappers;   //< Commands that map this artifact writable
  list<weak_ptr<Command>> _read_only_mappers;  //< Commands that map this artifact read-only

  static size_t next_id;
};

ostream& operator<<(ostream& o, const Artifact* f);
ostream& operator<<(ostream& o, const Artifact::VersionRef v);
