#include "core/Command.hh"

#include <cstdint>
#include <utility>

#include <fcntl.h>
#include <sys/stat.h>

#include <capnp/list.h>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/Serializer.hh"
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

shared_ptr<Command> Command::createChild(string exe, list<string> args,
                                         map<int, FileDescriptor> fds) {
  shared_ptr<Command> child(new Command(exe, args, fds, this));
  _children.push_back(child);

  INFO << this << " starting child " << child;
  LOG << "  " << exe << " (" << args.front() << ")";
  bool first = true;
  for (auto& arg : args) {
    if (!first) LOG << "    " << arg;
    first = false;
  }

  return child;
}

uint64_t Command::numDescendants() {
  uint64_t ret = 0;
  for (auto c : getChildren()) {
    ret += 1 + c->numDescendants();
  }
  return ret;
}

void Command::serialize(const Serializer& serializer, db::Command::Builder builder) {}
