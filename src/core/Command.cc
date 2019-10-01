#include "core/Command.hh"

#include <cstdint>
#include <limits>
#include <utility>

#include <fcntl.h>

#include <capnp/list.h>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/Serializer.hh"

using std::string;

std::shared_ptr<Command> Command::createChild(std::string cmd, const std::list<std::string>& args) {
  std::shared_ptr<Command> child(new Command(cmd, args, shared_from_this(), _depth + 1));
  _children.push_back(child);
  return child;
}

void Command::traceRead(std::shared_ptr<File> f) {
  // Record this command's dependency in the file
  f->traceRead(shared_from_this());
  
  // This file is now an input, unless we've also written to it
  if (_outputs.find(f) != _outputs.end()) {
    _inputs.insert(f);
  }
}

void Command::traceModify(std::shared_ptr<File> f) {
  // Record this command's effect on the file
  auto new_f = f->traceWrite(shared_from_this());
  
  // This file is now an output
  _outputs.insert(new_f);
}

void Command::traceCreate(std::shared_ptr<File> f) {
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

void Command::traceRemove(std::shared_ptr<File> f) {
  _deleted_files.insert(f);
  f = f->createVersion();
  f->setWriter(nullptr);
  f->setRemoved();
}

uint64_t Command::numDescendants() {
  uint64_t ret = 0;
  for (auto c : getChildren()) {
    ret += 1 + c->numDescendants();
  }
  return ret;
}

void Command::collapse(std::set<std::shared_ptr<Command>>& commands) {
  if (commands.empty()) return;
  size_t ansc_depth = std::numeric_limits<size_t>::max();

  // find the minimum common depth
  for (auto c : commands) {
    if (c->_depth < ansc_depth) {
      ansc_depth = c->_depth;
    }
  }
  bool fully_collapsed = false;
  while (!fully_collapsed) {
    // collapse all commands to this depth
    std::shared_ptr<Command> prev_command = nullptr;
    std::shared_ptr<Command> cur_command;
    fully_collapsed = true;
    for (auto c : commands) {
      cur_command = c->collapseHelper(ansc_depth);
      if (cur_command != prev_command && prev_command != nullptr) {
        fully_collapsed = false;
      }
      prev_command = cur_command;
    }
    ansc_depth--;
  }
}

std::shared_ptr<Command> Command::collapseHelper(unsigned int min_depth) {
  if (_depth > min_depth) {
    _collapse_with_parent = true;
    return _parent->collapseHelper(min_depth);
  } else {
    return shared_from_this();
  }
}

void Command::serialize(const Serializer& serializer, db::Command::Builder builder) {
  builder.setExecutable(_cmd);
  builder.setOutOfDate(false);
  builder.setCollapseWithParent(_collapse_with_parent);

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
