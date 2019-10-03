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
  Command(string cmd, const list<string>& args, shared_ptr<Command> parent) :
      _cmd(cmd),
      _args(args),
      _parent(parent) {}

 public:
  Command(string cmd, const list<string>& args) : _cmd(cmd), _args(args) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /****** Non-trivial methods ******/

  shared_ptr<Command> createChild(string cmd, const list<string>& args);

  size_t numDescendants();

  void addInput(shared_ptr<File> f) {
    if (_outputs.find(f) != _outputs.end()) _inputs.insert(f);
  }

  void addOutput(shared_ptr<File> f) { _outputs.insert(f); }

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
  list<shared_ptr<Command>> _children;
  set<shared_ptr<File>> _inputs;
  set<shared_ptr<File>> _outputs;
  set<shared_ptr<File>> _deleted_files;
  map<int, FileDescriptor> _initial_fds;
};
