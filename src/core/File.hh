#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <sys/stat.h>

#include "db/db.capnp.h"

class BuildGraph;
class Command;
class Serializer;

using std::enable_shared_from_this;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

class File : public enable_shared_from_this<File> {
 public:
  /// Alias for file types (see src/db/db.capnp)
  using Type = db::File::Type;

  /**
   * A Version holds information about a file in a particular state.
   *
   * A file may have many versions. For each version of a file, there is at most one writer, and an
   * arbitrary number of readers. Writing a file will create a new version, as will creating or
   * deleting a file. Creation and deletion are "special" types of writing that create new versions.
   *
   * While Processes will reference Files in their file descriptor tables, Commands depend on and
   * modify specific versions of files.
   *
   * A command that writes to a file without replacing all of its contents will have a read
   * dependency on the previous version, and creates a new version with updated contents. However,
   * if the command completely replaced the contents of the file (e.g. by opening it with O_TRUNC)
   * the command creates a new version without a read dependency on the old version.
   */
  class Version {
   public:
    // File should have access to File::Version fields
    friend class File;

    /// Track the types of actions that can create versions (see src/db/db.capnp)
    using Action = db::File::Version::Action::Which;

    Version(shared_ptr<File> file, size_t index, Action action, Command* writer) :
        _file(file),
        _index(index),
        _action(action),
        _writer(writer) {}

    shared_ptr<File> getFile() const { return _file; }
    size_t getIndex() const { return _index; }
    Action getAction() const { return _action; }

   private:
    shared_ptr<File> _file;  //< The file this is a version of
    size_t _index;           //< The index of this version
    Action _action;          //< The action that created this version
    Command* _writer;        //< The command that created this file version

    bool _has_metadata = false;
    struct stat _metadata;

    bool _has_fingerprint = false;
    vector<uint8_t> _fingerprint;
  };

  /****** Constructors ******/
  File(string path, Type type = Type::UNKNOWN) : _id(next_id++), _path(path), _type(type) {}

  // Disallow Copy
  File(const File&) = delete;
  File& operator=(const File&) = delete;

  // Allow Move
  File(File&&) = default;
  File& operator=(File&&) = default;

  /****** Non-trivial methods ******/

  void createdBy(Command* c);

  void readBy(Command* c);

  void writtenBy(Command* c);

  void truncatedBy(Command* c);

  void deletedBy(Command* c);

  void serialize(Serializer& serializer, db::File::Builder builder);

  /****** Getters and setters ******/

  size_t getId() const { return _id; }

  const string& getPath() const { return _path; }

  Type getType() const { return _type; }

  bool isLocal() const { return _path[0] != '/'; }

 private:
  Version& makeVersion(Version::Action a, Command* c = nullptr) {
    _versions.emplace_back(shared_from_this(), _versions.size(), a, c);
    return _versions.back();
  }

 private:
  size_t _id;
  string _path;                //< The absolute, normalized path to this file
  Type _type = Type::UNKNOWN;  //< The type of file being tracked
  vector<Version> _versions;   //< The sequence of versions of this file

  static size_t next_id;
};

ostream& operator<<(ostream& o, const File* f);
ostream& operator<<(ostream& o, const File::Version& v);
