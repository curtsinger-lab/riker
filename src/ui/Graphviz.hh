#pragma once

#include <fstream>
#include <map>
#include <string>

#include "core/Command.hh"
#include "core/File.hh"

using std::map;
using std::ofstream;
using std::string;

class Graphviz {
 public:
  Graphviz(string filename) : _out(filename) {
    _out << "digraph {\n";
    _out << "  graph [rankdir=LR]\n";
  }

  ~Graphviz() { _out << "}\n"; }

  void addCommand(Command* c) {
    if (_command_ids.find(c) == _command_ids.end()) {
      string id = "c" + to_string(_command_ids.size());
      _command_ids[c] = id;
      _out << "  " << id << " [label=\"" << c->getShortName() << "\" fontname=Courier]\n";
    }
  }

  void addFile(File* f) {
    if (_file_ids.find(f) == _file_ids.end()) {
      string id = "f" + to_string(_file_ids.size());
      _file_ids[f] = id;

      string parts = f->getShortName();
      for (auto& v : f->getVersions()) {
        string version_id = "v" + to_string(v.getIndex());
        _file_version_ids[&v] = id + ":" + version_id;
        
        string desc;
        switch (v.getAction()) {
          case File::Version::Action::CREATE:
            desc = "create";
            break;
          case File::Version::Action::REFERENCE:
            desc = "ref";
            break;
          case File::Version::Action::WRITE:
            desc = "write";
            break;
          case File::Version::Action::TRUNCATE:
            desc = "truncate";
            break;
          case File::Version::Action::DELETE:
            desc = "delete";
            break;
        }
        
        if (parts != "") parts += " | ";
        parts += "<" + version_id + "> " + version_id + ": " + desc;
      }

      string style;
      switch (f->getType()) {
        case File::Type::DIRECTORY:
          style = "dashed";
          break;
        case File::Type::PIPE:
          style = "rounded";
          break;
        default:
          style = "solid";
          break;
      }

      _out << "  " << id << " [label=\"" << parts << "\" shape=record style=\"" + style + "\"]\n";
    }
  }

  void addCommandEdge(Command* c1, Command* c2) {
    auto iter1 = _command_ids.find(c1);
    if (iter1 == _command_ids.end()) return;
    string& id1 = iter1->second;

    auto iter2 = _command_ids.find(c2);
    if (iter2 == _command_ids.end()) return;
    string& id2 = iter2->second;

    _out << "  " << id1 << " -> " << id2 << " [style=dashed weight=1]\n";
  }

  void addInputEdge(File::Version* f, Command* c) {
    addFile(f->getFile());

    string& id1 = _file_version_ids[f];
    string& id2 = _command_ids[c];

    _out << "  " << id1 << " -> " << id2 << " [arrowhead=empty weight=2]\n";
  }

  void addOutputEdge(Command* c, File::Version* f) {
    addFile(f->getFile());

    string& id1 = _command_ids[c];
    string& id2 = _file_version_ids[f];

    _out << "  " << id1 << " -> " << id2 << " [arrowhead=empty weight=2]\n";
  }

 private:
  ofstream _out;
  map<const Command*, string> _command_ids;
  map<const File*, string> _file_ids;
  map<const File::Version*, string> _file_version_ids;
};
