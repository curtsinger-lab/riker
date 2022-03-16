#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <utility>

class Artifact;
class Command;
class Version;

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
  std::string addCommand(std::shared_ptr<Command> c) noexcept;

  /// Add an artifact to the graph
  std::string addArtifact(std::shared_ptr<Artifact> c) noexcept;

  /// Add a version to the graph
  std::string addVersion(std::shared_ptr<Version> c) noexcept;

 private:
  /// Should the graph output include all artifacts?
  bool _show_all;

  /// A map from commands to their IDs in the graph output
  std::map<std::shared_ptr<Command>, std::string> _command_ids;

  /// A map from artifacts to the ID used to represent the entire artifact in the build graph
  std::map<std::shared_ptr<Artifact>, std::string> _artifact_ids;

  /// A map from versions to their ID (not prefixed by artifact ID)
  std::map<std::shared_ptr<Version>, std::string> _version_ids;

  /// A set of command edges, from parent to child
  std::set<std::pair<std::string, std::string>> _command_edges;

  // Command input edges (artifact -> command)
  std::set<std::pair<std::string, std::string>> _io_edges;

  // Versions that changed
  std::set<std::shared_ptr<Version>> _changed_versions;

  // Versions that may change (written by a command that must run or may run)
  std::set<std::shared_ptr<Version>> _may_change_versions;
};
