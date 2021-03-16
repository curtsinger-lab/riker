#include "Graph.hh"

#include <filesystem>
#include <list>
#include <memory>
#include <ostream>
#include <string>

#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;

namespace fs = std::filesystem;

/// Escape a string for safe printing inside a graphviz string
string escape(string s) noexcept {
  auto pos = s.find('"');
  if (pos == string::npos)
    return s;
  else
    return s.substr(0, pos) + "\\\"" + escape(s.substr(pos + 1));
}

string Graph::addCommand(shared_ptr<Command> c) noexcept {
  // Look to see if we already have a record of this command. Return if we do.
  auto iter = _command_ids.find(c);
  if (iter != _command_ids.end()) return iter->second;

  // Create an ID for this command and save it
  string command_id = "c" + to_string(_command_ids.size());
  _command_ids.emplace_hint(iter, c, command_id);

  // Add this command's children
  for (auto& child : c->getChildren()) {
    auto child_id = addCommand(child);
    _command_edges.emplace(command_id, child_id);
  }

  // Add this command's inputs
  for (const auto& [a, v, weak_creator] : c->getInputs()) {
    auto creator = weak_creator.lock();

    // Exclude artifacts with absolute paths, unless all artifacts are shown or the input was
    // created during the build
    if (fs::path(a->getName()).is_absolute() && !_show_all && !creator) continue;

    // Add the artifact and version
    auto artifact_id = addArtifact(a);
    auto version_id = addVersion(v);

    // Add the input edge
    _io_edges.emplace(artifact_id + ":" + version_id, command_id);
  }

  // Add this command's outputs
  for (const auto& [a, v] : c->getOutputs()) {
    // Add the artifact and version
    auto artifact_id = addArtifact(a);
    auto version_id = addVersion(v);

    // Add the output edge
    _io_edges.emplace(command_id, artifact_id + ":" + version_id);
  }

  // Return the new command's ID
  return command_id;
}

string Graph::addArtifact(shared_ptr<Artifact> a) noexcept {
  // Look for the artifact. If it's already in the map, return its ID.
  auto iter = _artifact_ids.find(a);
  if (iter != _artifact_ids.end()) return iter->second;

  // Create an ID for the artifact and save it
  string artifact_id = "a" + to_string(_artifact_ids.size());
  _artifact_ids.emplace_hint(iter, a, artifact_id);

  // Add all of this artifact's versions to the graph
  for (auto& v : a->getVersions()) {
    addVersion(v);
  }

  return artifact_id;
}

string Graph::addVersion(shared_ptr<Version> v) noexcept {
  // Look for the version. If it's already in the map, return its ID.
  auto iter = _version_ids.find(v);
  if (iter != _version_ids.end()) return iter->second;

  // Create an ID for the version and save it
  string version_id = "v" + to_string(_version_ids.size());
  _version_ids.emplace_hint(iter, v, version_id);

  return version_id;
}

ostream& operator<<(ostream& o, Graph& g) noexcept {
  o << "digraph {\n";
  o << "  graph [rankdir=LR]\n";

  // Create command vertices
  for (const auto& [c, id] : g._command_ids) {
    o << "  " << id << " [";
    o << "label=\"" << escape(c->getShortName()) << "\" ";
    o << "tooltip=\"" << escape(c->getFullName()) << "\" ";
    o << "fontname=Courier ";

    // Color the command based on its marking state
    if (c->getMarking() == RebuildMarking::MayRun) {
      // Commands that may run are yellow
      o << "style=\"filled\" ";
      o << "fillcolor=\"yellow\" ";

    } else if (c->getMarking() == RebuildMarking::MustRun) {
      // Commands that must run are red
      o << "style=\"filled\" ";
      o << "fillcolor=\"red\" ";

    } else if (c->getMarking() == RebuildMarking::AlreadyRun) {
      // Commands that were run in the production of the graph are green
      o << "style=\"filled\" ";
      o << "fillcolor=\"green\" ";
    }

    o << "]\n";
  }

  // Create command edges
  for (const auto& [parent, child] : g._command_edges) {
    o << "  " << parent << " -> " << child << " [style=dotted weight=1]\n";
  }

  // Create artifact vertices
  for (const auto& [artifact, artifact_id] : g._artifact_ids) {
    // Start the vertex with HTML output
    o << "  " << artifact_id << " [label=<";

    // Begin a table
    o << "<table border=\"0\" cellspacing=\"0\" cellborder=\"1\" cellpadding=\"5\">";

    // Print the artifact type
    o << "<tr><td border=\"0\"><sub>" << artifact->getTypeName() << "</sub></td></tr>";

    // Add a row with the artifact name, unless the artifact is unnamed
    auto name = artifact->getName();
    if (!name.empty()) {
      o << "<tr><td>" + name + "</td></tr>";
    }

    // Add a row for each version
    for (const auto& v : artifact->getVersions()) {
      o << "<tr><td port=\"" + g.addVersion(v) + "\"";
      /*if (_changed_versions.find(v) != _changed_versions.end()) {
        o << " bgcolor=\"yellow\"";
      }*/
      o << ">";
      o << "<font point-size=\"10\">" << v->getTypeName() << "</font>";
      o << "</td></tr>";
    }

    // Finish the vertex line
    o << "</table>> shape=plain]\n";
  }

  // Create I/O edges
  for (const auto& [src, dest] : g._io_edges) {
    // Does the reverse edges also appear in our set?
    if (auto iter = g._io_edges.find({dest, src}); iter != g._io_edges.end()) {
      // Yes. Draw this as a bidirectional edge
      o << "  " << src << " -> " << dest
        << " [arrowhead=empty weight=2 dir=both arrowtail=empty]\n";

      // Erase the other edge, since we've already drawn it
      g._io_edges.erase(iter);

    } else {
      // No, this is just a regular input edge
      o << "  " << src << " -> " << dest << " [arrowhead=empty weight=2]\n";
    }
  }

  // Close the graph
  o << "}\n";

  return o;
}
