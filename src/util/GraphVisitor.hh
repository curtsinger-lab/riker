#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>

#include "data/Command.hh"
#include "data/Version.hh"
#include "rebuild/Artifact.hh"
#include "rebuild/Env.hh"
#include "util/DependencyVisitor.hh"

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
class GraphVisitor : private DependencyVisitor {
 public:
  /**
   * Print graphviz output for a completed build
   * \param root           The root command in the build graph
   * \param show_sysfiles  If true, include artifacts that are system files
   */
  GraphVisitor(shared_ptr<Command> root, bool show_sysfiles) : _show_sysfiles(show_sysfiles) {
    processCommand(root);
  }

  /// Print the results of our stats gathering
  void print(ostream& o) {
    o << "digraph {\n";
    o << "  graph [rankdir=LR]\n";

    // Create command vertices
    for (auto& [c, id] : _command_ids) {
      o << "  " << id << " [";
      o << "label=\"" << escape(c->getShortName()) << "\" ";
      o << "tooltip=\"" << escape(c->getFullName()) << "\" ";
      o << "fontname=Courier ";
      if (_changed.find(c) != _changed.end()) {
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

      // Print the artifact type (not supported at the moment)
      // o << "<tr><td border=\"0\"><sub>" << ARTIFACT_TYPE << "</sub></td></tr>";

      // Add a row with the artifact name, if it has one
      if (auto path = artifact->getPath(); path.has_value()) {
        o << "<tr><td>" + escape(path.value()) + "</td></tr>";
      }

      // Add a row for each version
      size_t index = 0;
      for (auto v : artifact->getVersions()) {
        o << "<tr><td port=\"v" + to_string(index) + "\"";
        if (_changed_versions.find({artifact, index}) != _changed_versions.end()) {
          o << " bgcolor=\"yellow\"";
        }
        o << "></td></tr>";
        index++;
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

  bool isSystemFile(shared_ptr<Artifact> a) {
    auto path = a->getPath();
    if (!path.has_value()) return false;

    for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
      // Check if the path begins with one of our prefixes.
      // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
      if (path.value().rfind(p, 0) != string::npos) return true;
    }
    return false;
  }

  void processCommand(shared_ptr<Command> c) {
    // Add this command to the map of command IDs
    _command_ids.emplace(c, string("c") + to_string(c->getID()));

    // Emulate the command to gather its dependencies and children
    c->emulate(_env, *this);
  }

  void processArtifact(shared_ptr<Artifact> a) {
    // Add this artifact to the map of artifact IDs if necessary
    auto iter = _artifact_ids.find(a);
    if (iter == _artifact_ids.end()) {
      _artifact_ids.emplace_hint(iter, a, string("a") + to_string(_artifact_ids.size()));
    }
  }

  string getVersionID(shared_ptr<Artifact> a) {
    processArtifact(a);
    return _artifact_ids[a] + ":v" + to_string(a->getVersionCount() - 1);
  }

  virtual void addInput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {
    if (isSystemFile(a) && !_show_sysfiles) return;

    _io_edges.emplace(getVersionID(a), _command_ids[c]);
  }

  virtual void addOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {
    if (isSystemFile(a) && !_show_sysfiles) return;

    _io_edges.emplace(_command_ids[c], getVersionID(a));
  }

  virtual void launched(shared_ptr<Command> parent, shared_ptr<Command> child) override {
    // Process the child command to gather its dependencies
    processCommand(child);

    // Add the edge from parent to child
    _command_edges.emplace(_command_ids[parent], _command_ids[child]);
  }

  virtual void mismatch(shared_ptr<Artifact> a) override {
    _changed_versions.emplace(a, a->getVersionCount() - 1);
  }

  virtual void changed(shared_ptr<Command> c, shared_ptr<const Step> s) override {
    _changed.insert(c);
  }

 private:
  /// Should the graph output include system files?
  bool _show_sysfiles;

  /// The environment used to emulate the build trace
  Env _env;

  /// A map from commands to their IDs in the graph output
  map<shared_ptr<Command>, string> _command_ids;

  /// A map from versions to the ID used to represent the entire artifact in the build graph
  map<shared_ptr<Artifact>, string> _artifact_ids;

  /// A set of command edges, from parent to child
  set<pair<string, string>> _command_edges;

  /// A set of input/output edges, from source to destination (both inputs and outputs)
  set<pair<string, string>> _io_edges;

  /// The set of commands marked as changed
  set<shared_ptr<Command>> _changed;

  /// The set of artifact versions marked as changed
  set<pair<shared_ptr<Artifact>, size_t>> _changed_versions;
};
