#pragma once

#include <cstddef>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <unordered_set>

#include "core/Artifact.hh"

class Graphviz;
class Tracer;

using std::list;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::unordered_set;
using std::weak_ptr;

class Command : public std::enable_shared_from_this<Command> {
  /****** Constructors ******/
 private:
  Command(string exe, list<string> args, map<int, Artifact::Ref> initial_fds,
          shared_ptr<Command> parent) :
      _id(next_id++),
      _depth(parent->_depth + 1),
      _exe(exe),
      _args(args),
      _initial_fds(initial_fds),
      _parent(parent) {}

 public:
  Command(string exe, list<string> args, map<int, Artifact::Ref> initial_fds) :
      _id(next_id++), _depth(0), _exe(exe), _args(args), _initial_fds(initial_fds) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /****** Non-trivial methods ******/

  const string getShortName() const;

  shared_ptr<Command> createChild(string exe, list<string> args, map<int, Artifact::Ref> fds);

  /// Add an int edge from a file version to this command. Return true if this is a new edge.
  bool addInput(Artifact::VersionRef f) {
    if (_inputs.find(f) != _inputs.end()) return false;
    _inputs.insert(f);
    return true;
  }

  /// Add an output edge from this command to a file version. Return true if this is a new edge.
  bool addOutput(Artifact::VersionRef f) {
    if (_outputs.find(f) != _outputs.end()) return false;
    _outputs.insert(f);
    return true;
  }

  /// Run this command, or skip it and descend to its children if a run is unnecessary
  void run(Tracer& tracer);

  /// Clean up the graph by pruning unneeded edges and nodes
  /// If this returns true, the parent command can prune this command entirely
  bool prune();

  void drawGraph(Graphviz& g);

  /****** Getters and setters ******/

  size_t getId() const { return _id; }

  size_t getDepth() const { return _depth; }

  bool isRoot() const { return _depth == 0; }

  const string& getExecutable() const { return _exe; }

  const list<string>& getArguments() const { return _args; }

  const vector<shared_ptr<Command>>& getChildren() const { return _children; }

  const map<int, Artifact::Ref>& getInitialFDs() const { return _initial_fds; }

  void setInitialFDs(const map<int, Artifact::Ref>& fds) { _initial_fds = fds; }
  
  friend ostream& operator<<(ostream& o, const Command& c) {
    return o << "[Command " << c._id << " " << c.getShortName() << "]";
  }
  
  friend ostream& operator<<(ostream& o, const Command* c) {
    return o << *c;
  }

 private:
  size_t _id;
  size_t _depth;
  string _exe;
  list<string> _args;
  set<Artifact::VersionRef> _inputs;
  set<Artifact::VersionRef> _outputs;
  map<int, Artifact::Ref> _initial_fds;
  weak_ptr<Command> _parent;
  vector<shared_ptr<Command>> _children;

  static size_t next_id;
};
