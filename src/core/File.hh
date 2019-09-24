#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <sys/stat.h>

#include "db/db.capnp.h"

struct Command;
struct Process;
struct BuildGraph;

// TODO: Move this into the File class.
// Before that can happen, we'll need to update the code to inflate the loaded graph into File
// objects.
bool match_fingerprint(db::File::Reader file);

struct File : std::enable_shared_from_this<File> {
  /****** Constructors ******/

  File(BuildGraph& graph, size_t location, bool is_pipe, std::string path,
       std::shared_ptr<Command> creator = nullptr);

  // Disallow Copy
  File(const File&) = delete;
  File& operator=(const File&) = delete;

  // Allow Move
  File(File&&) = default;

  /****** Non-trivial methods ******/

  std::set<std::shared_ptr<Command>> collapse(unsigned int depth);

  std::shared_ptr<File> createVersion();

  void fingerprint();

  bool shouldSave();

  void serialize(db::File::Builder builder);

  std::shared_ptr<File> getLatestVersion();

  /****** Getters and setters ******/

  const std::string& getPath() const { return _path; }

  db::FileType getType() const { return _type; }
  bool isPipe() { return getType() == db::FileType::PIPE; }

  void setMode(uint16_t mode) { _mode = mode; }
  uint16_t getMode() const { return _mode; }

  size_t getLocation() const { return _location; }

  void addMmap(std::shared_ptr<Process> p) { _mmaps.insert(p); }
  void removeMmap(std::shared_ptr<Process> p) { _mmaps.erase(p); }

  const std::set<std::shared_ptr<Command>>& getReaders() const { return _readers; }
  void addReader(std::shared_ptr<Command> c) { _readers.insert(c); }

  const std::set<std::shared_ptr<Command>>& getInteractors() const { return _interactors; }
  void addInteractor(std::shared_ptr<Command> c) { _interactors.insert(c); }

  std::shared_ptr<Command> getCreator() const { return _creator; }
  void setCreator(std::shared_ptr<Command> c) { _creator = c; }
  bool isCreated() const { return getCreator() != nullptr; }

  std::shared_ptr<Command> getWriter() const { return _writer; }
  void setWriter(std::shared_ptr<Command> c) { _writer = c; }
  bool isWritten() const { return getWriter() != nullptr; }

  unsigned int getVersion() const { return _version; }
  bool isLatestVersion() const { return _next_version == nullptr; }

  std::shared_ptr<File> getPreviousVersion() const { return _prev_version; }
  bool hasPreviousVersion() const { return getPreviousVersion() != nullptr; }

  bool isRemoved() const { return _removed; }
  void setRemoved(bool r = true) { _removed = r; }

  void setFingerprintType(db::FingerprintType t) { _fingerprint_type = t; }
  db::FingerprintType getFingerprintType() const { return _fingerprint_type; }

 private:
  BuildGraph& _graph;  // A reference to the trace this file is part of
  size_t _location;    // This file's index in the BuildGraph::latest_versions map
  db::FileType _type;  // The type of file
  std::string _path;   // The path to this file
  uint16_t _mode;      // The file's access mode
  std::set<std::shared_ptr<Command>> _readers;      // Commands that read this file
  std::set<std::shared_ptr<Command>> _interactors;  // Commands that read OR modify this file
  std::set<std::shared_ptr<Process>> _mmaps;  // Processes that currently have an mmap of this file

  unsigned int _version = 0;  // The version number of this file
  std::shared_ptr<File> _prev_version;
  std::shared_ptr<File> _next_version;

  bool _removed = false;
  std::shared_ptr<Command> _creator;
  std::shared_ptr<Command> _writer;
  db::FingerprintType _fingerprint_type = db::FingerprintType::NONEXISTENT;
  struct stat _stat_info;
  std::vector<uint8_t> _checksum;
};
