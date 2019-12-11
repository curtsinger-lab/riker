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

using std::list;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::vector;

class Artifact {
 public:
  /// Types of artifacts we track
  enum class Type { UNKNOWN, REGULAR, DIRECTORY, SYMLINK, PIPE };

  /**
   * A Version holds information about an artifact in a particular state.
   *
   * An artifact may have many versions. For each version of an artifact, there is at most one
   * writer, and an arbitrary number of readers. Writing an artifact will create a new version, as
   * will creating or deleting an artifact. Creation and deletion are "special" types of writing
   * that create new versions.
   *
   * While Processes will reference Artifacts in their descriptor tables, Commands depend on
   * and modify specific versions of artifacts.
   *
   * A command that writes to an artifact without replacing all of its contents will have a read
   * dependency on the previous version, and creates a new version with updated contents. However,
   * if the command completely replaced the contents of the artifact (e.g. by opening it with
   * O_TRUNC) the command creates a new version without a read dependency on the old version.
   */
  class Version {
   public:
    // Artifact should have access to Artifact::Version fields
    friend class Artifact;

    /// Track the types of actions that can create versions
    enum class Action { CREATE, REFERENCE, WRITE, TRUNCATE, DELETE };

    Version(Artifact* artifact, size_t index, Action action, Command* writer) :
        _artifact(artifact), _index(index), _action(action), _writer(writer) {}

    void fingerprint();

    Artifact* getArtifact() const { return _artifact; }
    size_t getIndex() const { return _index; }
    Action getAction() const { return _action; }
    Command* getWriter() const { return _writer; }

    string getShortName() { return _artifact->getShortName() + " v" + to_string(_index); }

   private:
    Artifact* _artifact;  //< The artifact this is a version of
    size_t _index;        //< The index of this version
    Action _action;       //< The action that created this version
    Command* _writer;     //< The command that created this artifact version

    bool _has_metadata = false;
    struct stat _metadata;

    bool _has_fingerprint = false;
    vector<uint8_t> _fingerprint;
  };

  /****** Constructors ******/
  Artifact(string path, Type type = Type::UNKNOWN) : _id(next_id++), _path(path), _type(type) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /****** Non-trivial methods ******/

  void fingerprintIfNeeded(Command* modifier);

  /// Called during or after a command's creation of this artifact
  void createdBy(Command* c);

  /// Called during or after a command's read from this artifact
  void readBy(Command* c);

  /// Called just before allowing command c to write this artifact. May cache or fingerprint.
  void mayWrite(Command* c);

  /// Called after a command writes to this artifact
  void writtenBy(Command* c);

  /// Called just before allowing command c to truncate this artifact. May cache or fingerprint.
  void mayTruncate(Command* c);

  /// Called after a command truncates this artifact to zero length
  void truncatedBy(Command* c);

  /// Called just before allowing command c to delete this artifact. May cache or fingerprint.
  void mayDelete(Command* c);

  /// Called after a command unlinks this artifact
  void deletedBy(Command* c);

  /// Called just before allowing command c to mmap this artifact. May cache or fingerprint.
  void mayMap(Command* c, bool writable);

  /// Called after a command mmaps this artifact
  void mappedBy(Command* c, bool writable);

  /// Called after a command unmaps this artifact (partial unmaps should not call this function)
  void unmappedBy(Command* c, bool writable);

  /****** Getters and setters ******/

  size_t getId() const { return _id; }

  const string& getPath() const { return _path; }

  void updatePath(string path) { _path = path; }

  string getShortName() { return _path; }

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

  const list<Version>& getVersions() const { return _versions; }

 private:
  Version* makeVersion(Version::Action a, Command* c = nullptr);

 private:
  size_t _id;
  string _path;                      //< The absolute, normalized path to this artifact
  Type _type = Type::UNKNOWN;        //< The type of artifact being tracked
  list<Version> _versions;           //< The sequence of versions of this artifact
  set<Command*> _writable_mappers;   //< A set of commands with a writable mapping of this artifact
  set<Command*> _read_only_mappers;  //< A set of commands with a read-only mapping of this artifact

  static size_t next_id;
};

ostream& operator<<(ostream& o, const Artifact* f);
ostream& operator<<(ostream& o, const Artifact::Version* v);
