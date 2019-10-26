#include "core/Command.hh"

#include <iostream>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/Serializer.hh"
#include "ui/Graphviz.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::list;
using std::shared_ptr;
using std::string;

size_t Command::next_id = 0;

ostream& operator<<(ostream& o, const Command* c) {
  return o << "[Command " << c->getId() << " " << c->getShortName() << "]";
}

const string Command::getShortName() const {
  auto base = _exe;
  if (_args.size() > 0) base = _args.front();

  auto pos = base.rfind('/');
  if (pos == string::npos) {
    return base;
  } else {
    return base.substr(pos + 1);
  }
}

Command* Command::createChild(string exe, list<string> args, map<int, FileDescriptor> fds) {
  _children.push_back(Command(exe, args, fds, this));
  Command* child = &_children.back();

  INFO << this << " starting child " << child;
  if (args.size() > 0) {
    LOG << "  " << exe << " (" << args.front() << ")";
  } else {
    LOG << "  " << exe;
  }

  bool first = true;
  for (auto& arg : args) {
    if (!first) LOG << "    " << arg;
    first = false;
  }

  return child;
}

bool Command::prune() {
  // Remove inputs to commands that are also outputs
  for (auto iter = _inputs.begin(); iter != _inputs.end();) {
    auto input = *iter;
    if (_outputs.find(input) != _outputs.end()) {
      iter = _inputs.erase(iter);
    } else {
      ++iter;
    }
  }
  
  // Recursively prune in child commands, potentially removing the whole command
  for (auto iter = _children.begin(); iter != _children.end();) {
    auto& child = *iter;
    if (child.prune()) {
      iter = _children.erase(iter);
    } else {
      ++iter;
    }
  }
  
  // If this command has no childrena and no outputs, we can prune it
  return _outputs.size() == 0 && _children.size() == 0;
}

void Command::drawGraph(Graphviz& g) {
  g.addNode(this);
  for (auto f : _inputs) {
    if (!f->getFile()->isSystemFile() || options.show_sysfiles) {
      g.addEdge(f, this);
    }
  }
  for (auto f : _outputs) {
    if (!f->getFile()->isSystemFile() || options.show_sysfiles) {
      g.addEdge(this, f);
    }
  }
  for (auto& c : _children) {
    c.drawGraph(g);
    g.addEdge(this, &c);
  }
}

void Command::serialize(const Serializer& serializer, db::Command::Builder builder) const {}
