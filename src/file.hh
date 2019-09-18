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

  bool can_depend(Command* cmd);

  File* make_version();

  void fingerprint();

  void serialize(db::File::Builder builder);

  /****** Getters and setters ******/

  std::string getPath() { return blobToString(_serialized.getReader().getPath()); }

  bool isPipe() { return _serialized.getReader().getType() == db::FileType::PIPE; }

  void setMode(uint16_t mode) { _serialized.get().setMode(mode); }

  void setLatestVersion() { _serialized.get().setLatestVersion(true); }

  size_t getLocation() { return _location; }

  void addUser(Command* c) { _users.insert(c); }
  
  const std::set<Command*>& getUsers() { return _users; }
  
  size_t numUsers() { return _users.size(); }
  
  bool hasUsers() { return !_users.empty(); }
  
  bool isWritten() { return writer != nullptr; }
  
  bool isCreated() { return creator != nullptr; }
  
  bool shouldSave() {
    // Save files that have at least one user
    if (hasUsers()) return true;
    
    // Save files with a writer
    if (isWritten()) return true;
    
    // Save files with a creator
    if (isCreated()) return true;
    
    // Save files with a previous version that are not removed (CC: why?)
    if (prev_version != nullptr && !known_removed) return true;
    
    // Skip anything else
    return false;
  }

 private:
  Trace& _trace;
  size_t _location;
  capnp::Orphan<db::File> _serialized;
  std::set<Command*> _users;

 public:
  std::set<Process*> mmaps;
  std::list<Command*> interactions;
  std::list<Command*> conflicts;
  Command* creator;
  Command* writer;
  File* prev_version;
  unsigned int version;
  bool known_removed;
};
