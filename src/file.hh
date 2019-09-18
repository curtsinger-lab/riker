#pragma once

#include <list>
#include <set>

#include "db.capnp.h"

#include "util.hh"

struct Command;
struct Process;
struct Trace;

// TODO: Move this into the File class.
// Before that can happen, we'll need to update the code to inflate the loaded graph into File
// objects.
bool match_fingerprint(db::File::Reader file);

struct File {
  File(Trace& trace, size_t location, bool is_pipe, kj::ArrayPtr<const kj::byte> path,
       Command* creator, File* prev_version);

  std::set<Command*> collapse(unsigned int depth);

  bool canDependOn(Command* cmd);

  File* createVersion();

  void fingerprint();
  
  bool shouldSave();

  void serialize(db::File::Builder builder);

  /****** Getters and setters ******/

  std::string getPath() { return blobToString(_serialized.getReader().getPath()); }

  db::FileType getType() { return _serialized.getReader().getType(); }
  bool isPipe() { return getType() == db::FileType::PIPE; }

  void setMode(uint16_t mode) { _serialized.get().setMode(mode); }
  uint16_t getMode() { return _serialized.getReader().getMode(); }

  void setLatestVersion() { _serialized.get().setLatestVersion(true); }

  size_t getLocation() { return _location; }

  void addMmap(Process* p) { _mmaps.insert(p); }
  void removeMmap(Process* p) { _mmaps.erase(p); }

  const std::set<Command*>& getReaders() { return _readers; }
  void addReader(Command* c) { _readers.insert(c); }
  
  const std::set<Command*>& getInteractors() { return _interactors; }
  void addInteractor(Command* c) { _interactors.insert(c); }

  bool isWritten() { return writer != nullptr; }

  bool isCreated() { return creator != nullptr; }
  
  unsigned int getVersion() { return _version; }

 private:
  Trace& _trace;                        // A reference to the trace this file is part of
  size_t _location;                     // ???
  capnp::Orphan<db::File> _serialized;  // A serialized representation of this file
  std::set<Command*> _readers;          // Commands that read this file
  std::set<Command*> _interactors;      // Commands that read OR modify this file
  std::set<Process*> _mmaps;            // Processes that currently have an mmap of this file
  unsigned int _version;                // The version number of this file

 public:
  Command* creator;
  Command* writer;
  File* prev_version;
  bool known_removed;
};