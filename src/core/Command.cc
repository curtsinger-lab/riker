#include "core/Command.hh"

#include <cstdint>
#include <utility>

#include <fcntl.h>
#include <sys/stat.h>

#include <capnp/list.h>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/Serializer.hh"
#include "ui/Graphviz.hh"
#include "ui/log.hh"

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

void Command::drawGraph(Graphviz& g) {
  g.addNode(this);
  for (auto f : _inputs) {
    if (!f->getFile()->isSystemFile() || options.show_sysfiles) {
      g.addNode(f);
      g.addEdge(f, this);
    }
  }
  for (auto f : _outputs) {
    if (!f->getFile()->isSystemFile() || options.show_sysfiles) {
      g.addNode(f);
      g.addEdge(this, f);
    }
  }
  for (auto& c : _children) {
    c.drawGraph(g);
    g.addEdge(this, &c);
  }
}

void Command::serialize(const Serializer& serializer, db::Command::Builder builder) const {}
