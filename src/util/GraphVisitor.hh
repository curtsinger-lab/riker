#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>

#include "data/Command.hh"
#include "data/IR.hh"
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
class GraphVisitor {
 public:
  /**
   * Print graphviz output for a completed build
   * \param b              The build to analyze
   * \param show_sysfiles  If true, include artifacts that are system files
   */
  GraphVisitor(shared_ptr<Command> root, bool show_sysfiles) : _show_sysfiles(show_sysfiles) {
    visitCommand(root);
  }

  /// Print the results of our stats gathering
  void print(ostream& o) {
    o << "digraph {\n";
    o << "  graph [rankdir=LR]\n";

    // Create command vertices
    for (auto& [c, id] : _command_ids) {
      o << "  " << id << " [label=\"" << escape(c->getShortName()) << "\" tooltip=\""
        << escape(c->getFullName()) << "\" fontname=Courier]\n";
    }

    // Create command edges
    for (auto& [parent, child] : _command_edges) {
      o << "  " << parent << " -> " << child << " [style=dotted weight=1]\n";
    }

    // Create artifact vertices
    for (auto& a : _artifacts) {
      // Start the vertex with HTML output
      o << "  " << _artifact_ids[a] << " [label=<";

      // Begin a table
      o << "<table border=\"0\" cellspacing=\"0\" cellborder=\"1\" cellpadding=\"5\">";

      // Print the artifact type (not supported at the moment)
      // o << "<tr><td border=\"0\"><sub>" << ARTIFACT_TYPE << "</sub></td></tr>";

      // Add a row with the artifact name, if it has one
      if (auto path = a->getPath(); path.has_value()) {
        o << "<tr><td>" + escape(path.value()) + "</td></tr>";
      }

      // Add rows for artifact versions. Walk through them as a linked list
      auto current = a;
      while (current) {
        o << "<tr><td port=\"" + _version_ids[current] + "\"></td></tr>";
        current = current->getNext();
      }

      // Finish the vertex line
      o << "</table>> shape=plain]\n";
    }

    // Create I/O edges
    for (auto [src, dest] : _io_edges) {
      o << "  " << src << " -> " << dest << " [arrowhead=empty weight=2]\n";
    }

    o << "}\n";
  }

  friend ostream& operator<<(ostream& o, GraphVisitor v) {
    v.print(o);
    return o;
  }

 private:
  string escape(string s) {
    auto pos = s.find('"');
    if (pos == string::npos)
      return s;
    else
      return s.substr(0, pos) + "\\\"" + escape(s.substr(pos + 1));
  }

  bool isSystemFile(shared_ptr<Version> v) {
    auto path = v->getPath();
    if (!path.has_value()) return false;

    for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
      // Check if the path begins with one of our prefixes.
      // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
      if (path.value().rfind(p, 0) != string::npos) return true;
    }
    return false;
  }

  void visitCommand(shared_ptr<Command> c) {
    // Record this command with an ID
    _command_ids.emplace(c, string("c") + to_string(c->getID()));

    // Visit each of the steps the command runs
    for (auto s : c->getSteps()) {
      visitCommandStep(c, s);
    }
  };

  void visitCommandStep(shared_ptr<Command> c, shared_ptr<Step> s) {
    // Handle steps that launch new commands or access artifacts
    if (auto x = dynamic_pointer_cast<Launch>(s)) {
      // Recurse into the launched command
      visitCommand(x->getCommand());

      // Add the command edge
      _command_edges.emplace(_command_ids[c], _command_ids[x->getCommand()]);

    } else if (auto x = dynamic_pointer_cast<MetadataMatch>(s)) {
      visitInputEdge(c, x->getVersion());

    } else if (auto x = dynamic_pointer_cast<ContentsMatch>(s)) {
      visitInputEdge(c, x->getVersion());

    } else if (auto x = dynamic_pointer_cast<SetMetadata>(s)) {
      visitOutputEdge(c, x);

    } else if (auto x = dynamic_pointer_cast<SetContents>(s)) {
      visitOutputEdge(c, x);
    }
  }

  void visitInputEdge(shared_ptr<Command> c, shared_ptr<Version> v) {
    if (visitArtifact(v)) {
      _io_edges.emplace(_artifact_ids[v] + ":" + _version_ids[v], _command_ids[c]);
    }
  }

  void visitOutputEdge(shared_ptr<Command> c, shared_ptr<Version> v) {
    if (visitArtifact(v)) {
      _io_edges.emplace(_command_ids[c], _artifact_ids[v] + ":" + _version_ids[v]);
    }
  }

  bool visitArtifact(shared_ptr<Version> a) {
    // Get the first version of this artifact
    auto first = a->getFirstVersion();

    // If we've already added this artifact, just return true
    if (_artifacts.find(first) != _artifacts.end()) return true;

    // If this is a system file and we're not printing them, return false
    if (!_show_sysfiles && isSystemFile(first)) return false;

    // Add this original version to the set of artifacts
    _artifacts.insert(first);

    // Create an ID for this artifact
    string artifact_id = string("a") + to_string(_artifacts.size() - 1);

    // Walk through all versions of this artifact and record the artifact and version IDs
    int index = 0;
    auto current = first;
    while (current) {
      _artifact_ids[current] = artifact_id;
      _version_ids[current] = string("v") + to_string(index);
      current = current->getNext();
      index++;
    }

    return true;
  }

 private:
  bool _show_sysfiles;  //< Should the graph output include system files?

  /// A map from commands to their IDs in the graph output
  map<shared_ptr<Command>, string> _command_ids;

  /// A map from versions to the ID used to represent the entire artifact in the build graph
  map<shared_ptr<Version>, string> _artifact_ids;

  /// A map from versions to the version-specific ID used in the build graph
  /// Note: this ID does not include the artifact ID prefix
  map<shared_ptr<Version>, string> _version_ids;

  /// A set of all artifacts, identified by the first version of each
  set<shared_ptr<Version>> _artifacts;

  /// A set of command edges, from parent to child
  set<pair<string, string>> _command_edges;

  /// A set of input/output edges, from source to destination (both inputs and outputs)
  set<pair<string, string>> _io_edges;
};
