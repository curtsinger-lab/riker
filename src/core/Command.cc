#include "core/Command.hh"

#include <cstdint>
#include <utility>

#include <fcntl.h>
#include <sys/stat.h>

#include <capnp/list.h>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/Serializer.hh"

using std::list;
using std::shared_ptr;
using std::string;

shared_ptr<Command> Command::createChild(string cmd, list<string> args,
                                         map<int, FileDescriptor> fds) {
  shared_ptr<Command> child(new Command(cmd, args, fds, shared_from_this()));
  _children.push_back(child);
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
