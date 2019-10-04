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

using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

// TODO: Move this into the File class.
// Before that can happen, we'll need to update the code to inflate the loaded graph into File
// objects.
bool match_fingerprint(db::File::Reader file);

class File {
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
   private:
    // File should have access to File::Version fields
    friend class File;

    /// Track the types of actions that can create versions (see src/db/db.capnp)
    using Action = db::File::Version::Action::Which;

    /// Private constructor
    Version(Action action, shared_ptr<Command> writer) : _action(action), _writer(writer) {}

    /// Utility for making a shared_ptr to a Version
    static shared_ptr<Version> make_shared(Action a, shared_ptr<Command> c = nullptr) {
      return shared_ptr<Version>(new Version(a, c));
    }

   private:
    Action _action;                     //< The action that created this version
    shared_ptr<Command> _writer;        //< The command that created this file version
    set<shared_ptr<Command>> _readers;  //< A set of commands that read this file version

    bool _has_metadata = false;
    struct stat _metadata;

    bool _has_fingerprint = false;
    vector<uint8_t> _fingerprint;
  };

  /****** Constructors ******/
  File(string path, Type type = Type::UNKNOWN) : _path(path), _type(type) {}

  // Disallow Copy
  File(const File&) = delete;
  File& operator=(const File&) = delete;

  // Allow Move
  File(File&&) = default;
  File& operator=(File&&) = default;

  /****** Non-trivial methods ******/

  void createdBy(shared_ptr<Command> c);

  void readBy(shared_ptr<Command> c);

  void writtenBy(shared_ptr<Command> c);

  void truncatedBy(shared_ptr<Command> c);

  void deletedBy(shared_ptr<Command> c);

  void serialize(Serializer& serializer, db::File::Builder builder);

  /****** Getters and setters ******/

  const string& getPath() const { return _path; }

  bool isLocal() const { return _path[0] != '/'; }

 private:
  string _path;                           //< The absolute, normalized path to this file
  Type _type = Type::UNKNOWN;             //< The type of file being tracked
  vector<shared_ptr<Version>> _versions;  //< The sequence of versions of this file
};
