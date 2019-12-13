#pragma once

#include <fstream>
#include <map>
#include <string>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

using std::map;
using std::ofstream;
using std::string;
using std::to_string;

class Graphviz {
 public:
  Graphviz(string filename) : _out(filename) {
    _out << "digraph {\n";
    _out << "  graph [rankdir=LR]\n";
  }

  ~Graphviz() { _out << "}\n"; }

  void addCommand(shared_ptr<Command> c) {
    if (_command_ids.find(c) == _command_ids.end()) {
      string id = "c" + to_string(_command_ids.size());
      _command_ids[c] = id;
      _out << "  " << id << " [label=\"" << c->getShortName() << "\" fontname=Courier]\n";
    }
  }

  void addArtifact(shared_ptr<Artifact> f) {
    if (_file_ids.find(f) == _file_ids.end()) {
      string id = "f" + to_string(_file_ids.size());
      _file_ids[f] = id;

      string parts = "";

      parts +=
          "<table border=\"0\" cellspacing=\"0\" cellborder=\"1\" cellpadding=\"5\" "
          "style=\"rounded\">";
      
      parts += "<tr><td border=\"0\"><sub>" + f->getTypeName() + "</sub></td></tr>";

      if (f->getShortName() != "") {
        parts += "<tr><td>" + f->getShortName() + "</td></tr>";
      }

      for (auto v : f->getVersions()) {
        string version_id = "v" + to_string(v.getIndex());

        parts += "<tr><td port=\"" + version_id + "\">" + v.getActionName() + "</td></tr>";
      }

      parts += "</table>";

      _out << "  " << id << " [label=<" << parts << "> shape=plain]\n";
    }
  }

  void addCommandEdge(shared_ptr<Command> c1, shared_ptr<Command> c2) {
    auto iter1 = _command_ids.find(c1);
    if (iter1 == _command_ids.end()) return;
    string& id1 = iter1->second;

    auto iter2 = _command_ids.find(c2);
    if (iter2 == _command_ids.end()) return;
    string& id2 = iter2->second;

    _out << "  " << id1 << " -> " << id2 << " [style=dotted weight=1]\n";
  }

  void addInputEdge(Artifact::VersionRef f, shared_ptr<Command> c) {
    addArtifact(f.getArtifact());

    string file_id = _file_ids[f.getArtifact()];
    string id1 = file_id + ":v" + to_string(f.getIndex());
    string& id2 = _command_ids[c];

    _out << "  " << id1 << " -> " << id2 << " [arrowhead=empty weight=2]\n";
  }

  void addOutputEdge(shared_ptr<Command> c, Artifact::VersionRef f) {
    addArtifact(f.getArtifact());

    string& id1 = _command_ids[c];
    string file_id = _file_ids[f.getArtifact()];
    string id2 = file_id + ":v" + to_string(f.getIndex());

    _out << "  " << id1 << " -> " << id2 << " [arrowhead=empty weight=2]\n";
  }

 private:
  ofstream _out;
  map<shared_ptr<Command>, string> _command_ids;
  map<shared_ptr<Artifact>, string> _file_ids;
};
