#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>

#include "artifact/Artifact.hh"
#include "build/BuildObserver.hh"
#include "build/Env.hh"
#include "data/Command.hh"
#include "data/Version.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::map;
using std::ostream;
using std::pair;
using std::set;
using std::shared_ptr;
using std::to_string;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class Graph : public BuildObserver {
 public:
  /**
   * Print graphviz output for a build
   * \param show_sysfiles  If true, include artifacts that are system files
   */
  Graph(bool show_sysfiles) : _show_sysfiles(show_sysfiles) {}

  /// Print the results of our stats gathering
  ostream& print(ostream& o) {
    o << "digraph {\n";
    o << "  graph [rankdir=LR]\n";

    // Create command vertices
    for (auto& [c, id] : _command_ids) {
      o << "  " << id << " [";
      o << "label=\"" << escape(c->getShortName()) << "\" ";
      o << "tooltip=\"" << escape(c->getFullName()) << "\" ";
      o << "fontname=Courier ";
      if (_changed_commands.find(c) != _changed_commands.end()) {
        o << "style=\"filled\" ";
        o << "fillcolor=\"yellow\" ";
      }
      o << "]\n";
    }

    // Create command edges
    for (auto& [parent, child] : _command_edges) {
      o << "  " << parent << " -> " << child << " [style=dotted weight=1]\n";
    }

    // Create artifact vertices
    for (auto& [artifact, artifact_id] : _artifact_ids) {
      // Start the vertex with HTML output
      o << "  " << artifact_id << " [label=<";

      // Begin a table
      o << "<table border=\"0\" cellspacing=\"0\" cellborder=\"1\" cellpadding=\"5\">";

      // Print the artifact type
      o << "<tr><td border=\"0\"><sub>" << artifact->getTypeName() << "</sub></td></tr>";

      // Add a row with the artifact name, unless the artifact is unnamed
      if (!artifact->getName().empty()) {
        o << "<tr><td>" + artifact->getName() + "</td></tr>";
      }

      // Add a row for each version
      for (auto v : artifact->getVersions()) {
        o << "<tr><td port=\"" + getVersionID(v) + "\"";
        if (_changed_versions.find(v) != _changed_versions.end()) {
          o << " bgcolor=\"yellow\"";
        }
        o << ">";
        // o << "font point-size=\"10\">" << v->getTypeName() << "</font>";
        o << "</td></tr>";
      }

      // Finish the vertex line
      o << "</table>> shape=plain]\n";
    }

    // Create I/O edges for metadata accesses
    for (auto [src, dest] : _metadata_edges) {
      o << "  " << src << " -> " << dest << " [arrowhead=empty weight=2 label=\"m";

      // If this edge also appears in _content_edges, label it as both metadata and contents
      if (_content_edges.find({src, dest}) != _content_edges.end()) {
        o << "+c";
      }

      o << "\"]\n";
    }

    // Create I/O edges for content accesses
    for (auto [src, dest] : _content_edges) {
      // Only print this edge if it wasn't already printed as a metadata+content edge
      if (_metadata_edges.find({src, dest}) == _metadata_edges.end()) {
        o << "  " << src << " -> " << dest << " [arrowhead=empty weight=2 label=\"c\"]\n";
      }
    }

    o << "}\n";

    return o;
  }

  /// Print a Graph reference
  friend ostream& operator<<(ostream& o, Graph& g) { return g.print(o); }

  /// Print a Graph pointer
  friend ostream& operator<<(ostream& o, Graph* g) { return g->print(o); }

 private:
  /// Escape a string for safe printing inside a graphviz string
  string escape(string s) {
    auto pos = s.find('"');
    if (pos == string::npos)
      return s;
    else
      return s.substr(0, pos) + "\\\"" + escape(s.substr(pos + 1));
  }

  /// Check if an artifact appears to be a system file
  bool isSystemFile(shared_ptr<Artifact> a) {
    auto path = a->getName();

    for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
      // Check if the path begins with one of our prefixes.
      // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
      if (path.rfind(p, 0) != string::npos) return true;
    }
    return false;
  }

  string getCommandID(shared_ptr<Command> c) {
    // Add this command to the map of command IDs if necessary
    if (auto iter = _command_ids.find(c); iter == _command_ids.end()) {
      _command_ids.emplace_hint(iter, c, string("c") + to_string(_command_ids.size()));
    }
    return _command_ids[c];
  }

  string getArtifactID(shared_ptr<Artifact> a) {
    // Add this artifact to the map of artifact IDs if necessary
    if (auto iter = _artifact_ids.find(a); iter == _artifact_ids.end()) {
      _artifact_ids.emplace_hint(iter, a, string("a") + to_string(_artifact_ids.size()));
    }

    return _artifact_ids[a];
  }

  string getVersionID(shared_ptr<Version> v) {
    if (auto iter = _version_ids.find(v); iter == _version_ids.end()) {
      _version_ids.emplace_hint(iter, v, string("v") + to_string(_version_ids.size()));
    }
    return _version_ids[v];
  }

  string getVersionID(shared_ptr<Artifact> a, shared_ptr<Version> v) {
    return getArtifactID(a) + ":" + getVersionID(v);
  }

  /// Command c reads metadata from artifact a
  virtual void metadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a,
                             shared_ptr<Version> v) override {
    if (isSystemFile(a) && !_show_sysfiles) return;

    _metadata_edges.emplace(getVersionID(a, v), getCommandID(c));
  }

  /// Command c reads the contents of artifact a
  virtual void contentInput(shared_ptr<Command> c, shared_ptr<Artifact> a,
                            shared_ptr<Version> v) override {
    if (isSystemFile(a) && !_show_sysfiles) return;

    _content_edges.emplace(getVersionID(a, v), getCommandID(c));
  }

  /// Command c changes metadata for artifact a
  virtual void metadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a,
                              shared_ptr<Version> v) override {
    if (isSystemFile(a) && !_show_sysfiles) return;

    _metadata_edges.emplace(getCommandID(c), getVersionID(a, v));
  }

  /// Command c changes the contents of artifact a
  virtual void contentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a,
                             shared_ptr<Version> v) override {
    if (isSystemFile(a) && !_show_sysfiles) return;

    _content_edges.emplace(getCommandID(c), getVersionID(a, v));
  }

  /// Command c observes a change in metadata for artifact a
  virtual void metadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a,
                                shared_ptr<Version> observed,
                                shared_ptr<Version> expected) override {
    _changed_commands.insert(c);

    // The observed version is what the emulated build produced, and will appear in the graph.
    // Mark it as changed.
    _changed_versions.emplace(observed);
  }

  /// Command c observes a change in the contents of artifact a
  virtual void contentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a,
                               shared_ptr<Version> observed,
                               shared_ptr<Version> expected) override {
    _changed_commands.insert(c);

    // The observed version is what the emulated build produced, and will appear in the graph.
    // Mark it as changed.
    _changed_versions.emplace(observed);
  }

  /// Command c observes a change when executing an IR step
  virtual void commandChanged(shared_ptr<Command> c, shared_ptr<const Step> s) override {
    _changed_commands.insert(c);
  }

  /// The root command is starting
  virtual void launchRootCommand(shared_ptr<Command> root) override { getCommandID(root); }

  /// A child command is starting
  virtual void launchChildCommand(shared_ptr<Command> parent, shared_ptr<Command> child) override {
    // Add the edge from parent to child
    _command_edges.emplace(getCommandID(parent), getCommandID(child));
  }

  /// The metadata for an artifact on the file system do not match its state at the end of the build
  virtual void finalMetadataMismatch(shared_ptr<Artifact> a, shared_ptr<Version> observed,
                                     shared_ptr<Version> expected) override {
    // The expected version is what the emulated build produced. Mark it as changed.
    _changed_versions.emplace(observed);
  }

  /// The contents of an artifact on the file system do not match its state at the end of the build
  virtual void finalContentMismatch(shared_ptr<Artifact> a, shared_ptr<Version> observed,
                                    shared_ptr<Version> expected) override {
    // The observed version is what the emulated build produced. Mark this as changed.
    _changed_versions.emplace(observed);
  }

 private:
  /// Should the graph output include system files?
  bool _show_sysfiles;

  /// A map from commands to their IDs in the graph output
  map<shared_ptr<Command>, string> _command_ids;

  /// A map from artifacts to the ID used to represent the entire artifact in the build graph
  map<shared_ptr<Artifact>, string> _artifact_ids;

  /// A map from versions to their ID (not prefixed by artifact ID)
  map<shared_ptr<Version>, string> _version_ids;

  /// A set of command edges, from parent to child
  set<pair<string, string>> _command_edges;

  /// Input/output edges (source -> dest) that correspond to artifact metadata
  set<pair<string, string>> _metadata_edges;

  /// Input/output edges (source -> dest) that correspond to artifact contents
  set<pair<string, string>> _content_edges;

  /// The set of commands marked as changed
  set<shared_ptr<Command>> _changed_commands;

  /// The set of versions to mark as changed
  set<shared_ptr<Version>> _changed_versions;
};
