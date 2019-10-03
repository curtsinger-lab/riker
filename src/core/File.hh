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
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

// TODO: Move this into the File class.
// Before that can happen, we'll need to update the code to inflate the loaded graph into File
// objects.
bool match_fingerprint(db::File::Reader file);

class File : public enable_shared_from_this<File> {
 public:
  /****** Constructors ******/
  File(BuildGraph& graph, size_t location, db::FileType type, string path,
       shared_ptr<Command> creator = nullptr) :
      _graph(graph),
      _location(location),
      _type(type),
      _path(path),
      _creator(creator) {}

  // Disallow Copy
  File(const File&) = delete;
  File& operator=(const File&) = delete;

  // Allow Move
  File(File&&) = default;

  /****** Non-trivial methods ******/

  shared_ptr<File> createVersion(shared_ptr<Command> creator = nullptr);

  void fingerprint();

  bool shouldSave() const;

  void serialize(Serializer& serializer, db::File::Builder builder);

  shared_ptr<File> getLatestVersion();

  bool isModified() const;

  bool isLocal() const;

  shared_ptr<File> traceWrite(shared_ptr<Command> c);
  
  shared_ptr<File> traceRemove(shared_ptr<Command> c);

  /****** Getters and setters ******/

  const string& getPath() const { return _path; }

  bool isPipe() const { return _type == db::FileType::PIPE; }

  void setMode(uint16_t mode) { _mode = mode; }

  size_t getLocation() const { return _location; }

  void addMmap(shared_ptr<Command> c) { _mmaps.insert(c); }
  void removeMmap(shared_ptr<Command> c) { _mmaps.erase(c); }

  const set<shared_ptr<Command>>& getReaders() const { return _readers; }
  void addReader(shared_ptr<Command> c) { _readers.insert(c); }
  bool isRead() const { return !_readers.empty(); }

  shared_ptr<Command> getCreator() const { return _creator; }
  bool isCreated() const { return getCreator() != nullptr; }

  shared_ptr<Command> getWriter() const { return _writer; }
  bool isWritten() const { return getWriter() != nullptr; }

  bool isRemoved() const { return _removed; }

 private:
  BuildGraph& _graph;                 // A reference to the trace this file is part of
  size_t _location;                   // This file's index in the BuildGraph::latest_versions map
  db::FileType _type;                 // The type of file
  string _path;                       // The path to this file
  uint16_t _mode;                     // The file's access mode
  set<shared_ptr<Command>> _readers;  // Commands that read this file
  set<shared_ptr<Command>> _mmaps;    // Commands that currently have an mmap of this file

  shared_ptr<File> _prev_version;
  shared_ptr<File> _next_version;

  bool _removed = false;
  shared_ptr<Command> _creator;
  shared_ptr<Command> _writer = nullptr;

  db::FingerprintType _fingerprint_type = db::FingerprintType::NONEXISTENT;
  struct stat _stat_info;
  vector<uint8_t> _checksum;
};
