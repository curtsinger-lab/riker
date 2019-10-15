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
    _out << "  node [fontname=Courier]\n";
  }

  ~Graphviz() { _out << "}\n"; }

  void addNode(Command* c) {
    if (_command_ids.find(c) == _command_ids.end()) {
      string id = "c" + to_string(_command_ids.size());
      _command_ids[c] = id;
      _out << "  \"" << id << "\" [label=\"" << c->getShortName() << "\"]\n";
    }
  }

  void startSubgraph(File* f) {
    if (_file_ids.find(f) == _file_ids.end()) {
      string id = "cluster_f" + to_string(_file_ids.size());
      _file_ids[f] = id;
      _out << "  subgraph " << id << " {\n";
      _out << "    label=\"" << f->getShortName() << "\"\n";
    }
  }

  void finishSubgraph() { _out << "  }\n"; }

  void addNode(File::Version* f, bool fullname = false) {
    if (_file_version_ids.find(f) == _file_version_ids.end()) {
      string id = "v" + to_string(_file_version_ids.size());
      _file_version_ids[f] = id;

      string shape;
      switch (f->getFile()->getType()) {
        case File::Type::DIRECTORY:
          shape = "folder";
          break;
        case File::Type::PIPE:
          shape = "diamond";
          break;
        default:
          shape = "rectangle";
          break;
      }
      
      string label = "v" + to_string(f->getIndex());
      if (fullname) label = f->getFile()->getShortName();

      _out << "  \"" << id << "\" [label=\"" << label << "\" shape=" << shape << "]\n";
    }
  }

  void addEdge(Command* c1, Command* c2) {
    auto iter1 = _command_ids.find(c1);
    if (iter1 == _command_ids.end()) return;
    string& id1 = iter1->second;

    auto iter2 = _command_ids.find(c2);
    if (iter2 == _command_ids.end()) return;
    string& id2 = iter2->second;

    _out << "  \"" << id1 << "\" -> \"" << id2 << "\" [style=dashed]\n";
  }

  void addEdge(File::Version* f, Command* c) {
    auto iter1 = _file_version_ids.find(f);
    if (iter1 == _file_version_ids.end()) return;
    string& id1 = iter1->second;

    auto iter2 = _command_ids.find(c);
    if (iter2 == _command_ids.end()) return;
    string& id2 = iter2->second;

    _out << "  \"" << id1 << "\" -> \"" << id2 << "\" "
         << "[arrowhead=empty]\n";
  }

  void addEdge(Command* c, File::Version* f) {
    auto iter1 = _command_ids.find(c);
    if (iter1 == _command_ids.end()) return;
    string& id1 = iter1->second;

    auto iter2 = _file_version_ids.find(f);
    if (iter2 == _file_version_ids.end()) return;
    string& id2 = iter2->second;

    string label;
    switch (f->getAction()) {
      case File::Version::Action::CREATE:
        label = "create";
        break;
      case File::Version::Action::REFERENCE:
        label = "reference";
        break;
      case File::Version::Action::WRITE:
        label = "write";
        break;
      case File::Version::Action::TRUNCATE:
        label = "truncate";
        break;
      case File::Version::Action::DELETE:
        label = "delete";
        break;
    }

    _out << "  \"" << id1 << "\" -> \"" << id2 << "\" "
         << "[arrowhead=empty label=\"" << label << "\"]\n";
  }

  void addEdge(File::Version* f1, File::Version* f2) {
    auto iter1 = _file_version_ids.find(f1);
    if (iter1 == _file_version_ids.end()) return;
    string& id1 = iter1->second;

    auto iter2 = _file_version_ids.find(f2);
    if (iter2 == _file_version_ids.end()) return;
    string& id2 = iter2->second;

    _out << "  \"" << id1 << "\" -> \"" << id2 << "\" [style=invis]\n";
  }

 private:
  ofstream _out;
  map<Command*, string> _command_ids;
  map<File*, string> _file_ids;
  map<File::Version*, string> _file_version_ids;
};
