#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>

using std::map;
using std::set;
using std::shared_ptr;
using std::string;

class Artifact;
class Command;
class MetadataVersion;
class ContentVersion;

class Graph {
 public:
  /// Create a graph from the commands in an input trace
  Graph(bool show_all) noexcept : _show_all(show_all) {}

  /// Print a Graph reference
  friend std::ostream& operator<<(std::ostream& o, Graph& g) noexcept;

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
  string addVersion(shared_ptr<MetadataVersion> c) noexcept;

  /// Add a version to the graph
  string addVersion(shared_ptr<ContentVersion> c) noexcept;

 private:
  /// Should the graph output include all artifacts?
  bool _show_all;

  /// A map from commands to their IDs in the graph output
  map<shared_ptr<Command>, string> _command_ids;

  /// A map from artifacts to the ID used to represent the entire artifact in the build graph
  map<shared_ptr<Artifact>, string> _artifact_ids;

  /// A map from versions to their ID (not prefixed by artifact ID)
  map<shared_ptr<MetadataVersion>, string> _metadata_version_ids;

  /// A map from versions to their ID (not prefixed by artifact ID)
  map<shared_ptr<ContentVersion>, string> _content_version_ids;

  /// A set of command edges, from parent to child
  set<std::pair<string, string>> _command_edges;

  // Command input edges (artifact -> command)
  set<std::pair<string, string>> _io_edges;
};
