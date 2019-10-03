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

shared_ptr<Command> Command::createChild(string cmd, const list<string>& args) {
  shared_ptr<Command> child(new Command(cmd, args, shared_from_this()));
  _children.push_back(child);
  return child;
}

void Command::addInput(shared_ptr<File> f) {
  // This file is now an input, unless we've also written to it
  if (_outputs.find(f) != _outputs.end()) {
    _inputs.insert(f);
  }
}

void Command::traceModify(shared_ptr<File> f) {
  // Record this command's effect on the file
  auto new_f = f->traceWrite(shared_from_this());

  // This file is now an output
  _outputs.insert(new_f);
}

void Command::traceCreate(shared_ptr<File> f) {
  if (f->isCreated() && !f->isWritten()) {
    bool file_exists;
    if (f->isRemoved() || f->isPipe()) {
      file_exists = false;
    } else {
      struct stat stat_info;
      file_exists = (lstat(f->getPath().c_str(), &stat_info) == 0);
    }

    if (!file_exists) {
      f->createVersion(shared_from_this());
      f->setRemoved(false);
    }
  }
}

void Command::traceRemove(shared_ptr<File> f) {
  _deleted_files.insert(f);
  f = f->createVersion();
  f->setRemoved();
}

uint64_t Command::numDescendants() {
  uint64_t ret = 0;
  for (auto c : getChildren()) {
    ret += 1 + c->numDescendants();
  }
  return ret;
}

void Command::serialize(const Serializer& serializer, db::Command::Builder builder) {
  builder.setExecutable(_cmd);
  builder.setOutOfDate(false);

  auto args_output = builder.initArgv(_args.size());
  size_t args_index = 0;
  for (auto arg : _args) {
    args_output.set(args_index, arg);
    args_index++;
  }

  size_t initial_fd_count = 0;
  for (auto iter : _initial_fds) {
    if (serializer.hasFile(iter.second.file)) {
      initial_fd_count++;
    }
  }

  auto initial_fds_output = builder.initInitialFDs(initial_fd_count);

  size_t initial_fd_index = 0;
  for (auto iter : _initial_fds) {
    if (serializer.hasFile(iter.second.file)) {
      auto output = initial_fds_output[initial_fd_index];
      initial_fd_index++;

      output.setFd(iter.first);
      output.setFileID(serializer.getFileIndex(iter.second.file));
      output.setCanRead(iter.second.access_mode != O_WRONLY);
      output.setCanWrite(iter.second.access_mode != O_RDONLY);
    }
  }

  // This relies on the order of commands within the graph file.
  // Every command must be followed by all of its descendants.
  // This is true because of the implementation of Serializer::addCommand, but splitting the logic
  // up across these two files seems risky.
  builder.setDescendants(numDescendants());
}
