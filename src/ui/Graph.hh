#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>

using std::map;
using std::ostream;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;

class Artifact;
class Command;
class InputTrace;
class Version;

class Graph {
 public:
  /// Create a graph from the commands in an input trace
  Graph(bool show_all) noexcept : _show_all(show_all) {}

  /// Print a Graph reference
  friend ostream& operator<<(ostream& o, Graph& g) noexcept;

  /// Add an iterable set of commands to the graph
  template <class T>
  void addCommands(const T& commands) noexcept {
    for (const auto& c : commands) {
      addCommand(c);
    }
  }

 private:
  /// Add a command to the graph
  string addCommand(shared_ptr<Command> c) noexcept;

  /// Add an artifact to the graph
  string addArtifact(shared_ptr<Artifact> c) noexcept;

  /// Add a version to the graph
  string addVersion(shared_ptr<Version> c) noexcept;

 private:
  /// Should the graph output include all artifacts?
  bool _show_all;

  /// A map from commands to their IDs in the graph output
  map<shared_ptr<Command>, string> _command_ids;

  /// A map from artifacts to the ID used to represent the entire artifact in the build graph
  map<shared_ptr<Artifact>, string> _artifact_ids;

  /// A map from versions to their ID (not prefixed by artifact ID)
  map<shared_ptr<Version>, string> _version_ids;

  /// A set of command edges, from parent to child
  set<pair<string, string>> _command_edges;

  // Command input edges (artifact -> command)
  set<pair<string, string>> _io_edges;
};
