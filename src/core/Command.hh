#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/db.capnp.h"

class Serializer;

using std::list;
using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;

class Command;
ostream& operator<<(ostream& o, const Command* c);

class Command {
  /****** Constructors ******/
 private:
  Command(string exe, list<string> args, map<int, FileDescriptor> fds, Command* parent) :
      _id(next_id++),
      _depth(parent->_depth + 1),
      _exe(exe),
      _args(args),
      _initial_fds(fds) {}

 public:
  Command(string exe, list<string> args, map<int, FileDescriptor> fds) :
      _id(next_id++),
      _depth(0),
      _exe(exe),
      _args(args),
      _initial_fds(fds) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /****** Non-trivial methods ******/

  const string getShortName() const;

  shared_ptr<Command> createChild(string exe, list<string> args, map<int, FileDescriptor> fds);

  size_t numDescendants();

  /// Add an int edge from a file version to this command. Return true if this is a new edge.
  bool addInput(shared_ptr<File::Version> f) {
    if (_inputs.find(f) != _inputs.end()) return false;
    _inputs.insert(f);
    return true;
  }

  /// Add an output edge from this command to a file version. Return true if this is a new edge.
  bool addOutput(shared_ptr<File::Version> f) {
    if (_outputs.find(f) != _outputs.end()) return false;
    _outputs.insert(f);
    return true;
  }

  void serialize(const Serializer& serializer, db::Command::Builder builder);

  /****** Getters and setters ******/

  size_t getId() const { return _id; }

  size_t getDepth() const { return _depth; }

  const string& getExecutable() const { return _exe; }

  const list<string>& getArguments() const { return _args; }

  const list<shared_ptr<Command>>& getChildren() const { return _children; }

  const map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }

  void setInitialFDs(const map<int, FileDescriptor>& fds) { _initial_fds = fds; }

 private:
  size_t _id;
  size_t _depth;
  string _exe;
  list<string> _args;
  list<shared_ptr<Command>> _children;
  set<shared_ptr<File::Version>> _inputs;
  set<shared_ptr<File::Version>> _outputs;
  map<int, FileDescriptor> _initial_fds;

  static size_t next_id;
};
