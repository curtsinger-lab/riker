#pragma once

#include <list>
#include <set>

#include "db.capnp.h"

#include "util.hh"

struct Command;
struct Process;
struct Trace;

// TODO: Move this into the File class.
// Before that can happen, we'll need to update the code to inflate the loaded graph into File objects.
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

 private:
  Trace& _trace;
  size_t _location;
  capnp::Orphan<db::File> _serialized;

 public:
  std::set<Command*> users;
  std::set<Process*> mmaps;
  std::list<Command*> interactions;
  std::list<Command*> conflicts;
  Command* creator;
  Command* writer;
  File* prev_version;
  unsigned int version;
  bool known_removed;
};
