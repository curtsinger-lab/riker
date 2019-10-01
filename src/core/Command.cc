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

Command::Command(std::string cmd, const std::list<std::string>& args,
                 std::shared_ptr<Command> parent, unsigned int depth) :
    _cmd(cmd),
    _args(args),
    _parent(parent),
    _depth(depth) {}

Command::Command(std::string cmd, const std::list<std::string>& args) :
    _cmd(cmd),
    _args(args),
    _parent(nullptr),
    _depth(0) {}

std::shared_ptr<Command> Command::createChild(std::string cmd, const std::list<std::string>& args) {
  std::shared_ptr<Command> child(new Command(cmd, args, shared_from_this(), _depth + 1));
  _children.push_back(child);
  return child;
}

void Command::traceRead(std::shared_ptr<File> f) {
  if (canDependOn(f)) addInput(f);
}

void Command::traceModify(std::shared_ptr<File> f) {
  addOutput(f);
}

void Command::addInput(std::shared_ptr<File> f) {
  // search through all files, do versioning
  f->addInteractor(shared_from_this());
  f->addReader(shared_from_this());
  _inputs.insert(f);

  // No checking for races on pipes
  if (f->isPipe()) return;

  // If we've read from this file before, check for a race
  // TODO: Use set find
  for (auto rd : _rd_interactions) {
    if (f->getLocation() == rd->getLocation() && f->getWriter().get() != this) {
      if (f->getVersion() == rd->getVersion()) {
        return;
      } else /* we've found a race */ {
        std::set<std::shared_ptr<Command>> conflicts = f->collapse(rd->getVersion());
        Command::collapse(conflicts);
      }
    }
  }
  // mark that we've now interacted with this version of the file
  _rd_interactions.insert(f);
}

void Command::addOutput(std::shared_ptr<File> f) {
  // if we've written to the file before, check for a race
  if (!f->isPipe()) {
    for (auto wr : _wr_interactions) {
      if (f->getLocation() == wr->getLocation()) {
        _outputs.insert(f);
        if (f->getVersion() == wr->getVersion()) {
          return;
        } else {
          std::set<std::shared_ptr<Command>> conflicts = f->collapse(wr->getVersion());
          Command::collapse(conflicts);
          return;
        }
      }
    }
  }

  f->addInteractor(shared_from_this());
  // if we haven't written to this file before, create a new version
  std::shared_ptr<File> fnew;
  if ((f->isCreated() && !f->isWritten()) || f->getWriter().get() == this) {
    // Unless we just created it, in which case it is pristine
    fnew = f;
  } else {
    fnew = f->createVersion();
  }
  fnew->setWriter(shared_from_this());
  _outputs.insert(fnew);
  _wr_interactions.insert(fnew);
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

bool Command::canDependOn(const std::shared_ptr<File> f) {
  // If this command is the only writer, it cannot depend on the file

  if (f->getWriter().get() == this) return false;

  // If the file is not written and was created by this command, it cannot depend on the file
  if (!f->isWritten() && f->getCreator().get() == this) return false;

  // Otherwise the command can depend on the file
  return true;
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
