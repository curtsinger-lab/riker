#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "core/FileDescriptor.hh"
#include "db/db.capnp.h"

class File;
class Serializer;

using std::enable_shared_from_this;
using std::list;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;

class Command : public enable_shared_from_this<Command> {
  /****** Constructors ******/
 private:
  Command(string cmd, const list<string>& args, shared_ptr<Command> parent, unsigned int depth) :
      _cmd(cmd),
      _args(args),
      _parent(parent),
      _depth(depth) {}

 public:
  Command(string cmd, const list<string>& args) :
      _cmd(cmd),
      _args(args),
      _parent(nullptr),
      _depth(0) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /****** Non-trivial methods ******/

  shared_ptr<Command> createChild(string cmd, const list<string>& args);

  size_t numDescendants();

  void traceRead(shared_ptr<File> f);

  void traceModify(shared_ptr<File> f);

  void traceCreate(shared_ptr<File> f);

  void traceRemove(shared_ptr<File> f);

  void serialize(const Serializer& serializer, db::Command::Builder builder);

  /****** Getters and setters ******/

  const string& getExecutable() const { return _cmd; }

  const list<string>& getArguments() const { return _args; }

  const list<shared_ptr<Command>>& getChildren() const { return _children; }

  const set<shared_ptr<File>>& getDeletedFiles() const { return _deleted_files; }

  const map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }

  void setInitialFDs(const map<int, FileDescriptor>& fds) { _initial_fds = fds; }

 private:
  string _cmd;
  list<string> _args;
  shared_ptr<Command> _parent;
  unsigned int _depth;
  list<shared_ptr<Command>> _children;
  set<shared_ptr<File>> _inputs;
  set<shared_ptr<File>> _outputs;
  set<shared_ptr<File>> _deleted_files;
  bool _collapse_with_parent;
  map<int, FileDescriptor> _initial_fds;
};
