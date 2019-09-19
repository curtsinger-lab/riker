#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <set>

#include <capnp/orphan.h>
#include <kj/string.h>

#include "db/db.capnp.h"

struct Command;
struct Process;
struct BuildGraph;

// TODO: Move this into the File class.
// Before that can happen, we'll need to update the code to inflate the loaded graph into File
// objects.
bool match_fingerprint(db::File::Reader file);

struct File {
  File(BuildGraph& trace, size_t location, bool is_pipe, kj::StringPtr path, Command* creator,
       File* prev_version);

  std::set<Command*> collapse(unsigned int depth);

  File* createVersion();

  void fingerprint();

  bool shouldSave();

  void serialize(db::File::Builder builder);

  /****** Getters and setters ******/

  kj::StringPtr getPath() const { return _serialized.getReader().getPath(); }

  db::FileType getType() { return _serialized.getReader().getType(); }
  bool isPipe() { return getType() == db::FileType::PIPE; }

  void setMode(uint16_t mode) { _serialized.get().setMode(mode); }
  uint16_t getMode() { return _serialized.getReader().getMode(); }

  void setLatestVersion() { _serialized.get().setLatestVersion(true); }

  size_t getLocation() { return _location; }

  void addMmap(std::shared_ptr<Process> p) { _mmaps.insert(p); }
  void removeMmap(std::shared_ptr<Process> p) { _mmaps.erase(p); }

  const std::set<Command*>& getReaders() { return _readers; }
  void addReader(Command* c) { _readers.insert(c); }

  const std::set<Command*>& getInteractors() const { return _interactors; }
  void addInteractor(Command* c) { _interactors.insert(c); }

  Command* getCreator() const { return _creator; }
  void setCreator(Command* c) { _creator = c; }
  bool isCreated() const { return getCreator() != nullptr; }

  Command* getWriter() const { return _writer; }
  void setWriter(Command* c) { _writer = c; }
  bool isWritten() const { return getWriter() != nullptr; }

  unsigned int getVersion() const { return _version; }

  File* getPreviousVersion() const { return _prev_version; }
  bool hasPreviousVersion() const { return getPreviousVersion() != nullptr; }

  bool isRemoved() const { return _removed; }
  void setRemoved(bool r = true) { _removed = r; }

 private:
  BuildGraph& _trace;                         // A reference to the trace this file is part of
  size_t _location;                           // ???
  capnp::Orphan<db::File> _serialized;        // A serialized representation of this file
  std::set<Command*> _readers;                // Commands that read this file
  std::set<Command*> _interactors;            // Commands that read OR modify this file
  std::set<std::shared_ptr<Process>> _mmaps;  // Processes that currently have an mmap of this file
  unsigned int _version;                      // The version number of this file
  File* _prev_version;
  bool _removed = false;
  Command* _creator;
  Command* _writer = nullptr;
};
