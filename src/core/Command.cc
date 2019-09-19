#include "core/Command.hh"

#include <cstdint>

#include "core/File.hh"
#include "core/FileDescriptor.hh"

Command::Command(BuildGraph& state, std::string cmd, Command* parent, unsigned int depth) :
    _state(state),
    _cmd(cmd),
    _parent(parent),
    _depth(depth) {}

Command* Command::createChild(std::string cmd) {
  Command* child = new Command(_state, cmd, this, _depth + 1);
  _children.push_back(child);
  return child;
}

void Command::addInput(File* f) {
  // search through all files, do versioning
  f->addInteractor(this);
  f->addReader(this);
  this->_inputs.insert(f);

  // No checking for races on pipes
  if (f->isPipe()) return;

  // If we've read from this file before, check for a race
  // TODO: Use set find
  for (auto rd : this->rd_interactions) {
    if (f->getLocation() == rd->getLocation() && f->getWriter() != this) {
      if (f->getVersion() == rd->getVersion()) {
        return;
      } else /* we've found a race */ {
        std::set<Command*> conflicts = f->collapse(rd->getVersion());
        this->collapse(&conflicts);
      }
    }
  }
  // mark that we've now interacted with this version of the file
  this->rd_interactions.insert(f);
}

void Command::addOutput(File* f, size_t file_location) {
  // if we've written to the file before, check for a race
  if (!f->isPipe()) {
    for (auto wr : this->wr_interactions) {
      if (f->getLocation() == wr->getLocation()) {
        this->_outputs.insert(f);
        if (f->getVersion() == wr->getVersion()) {
          return;
        } else {
          std::set<Command*> conflicts = f->collapse(wr->getVersion());
          this->collapse(&conflicts);
          // wr->has_race = true;
          // this->has_race = true;
          return;
        }
      }
    }
  }

  f->addInteractor(this);
  // if we haven't written to this file before, create a new version
  File* fnew;
  if ((f->isCreated() && !f->isWritten()) || f->getWriter() == this) {
    // Unless we just created it, in which case it is pristine
    fnew = f;
  } else {
    fnew = f->createVersion();
    fnew->setCreator(nullptr);
  }
  fnew->setWriter(this);
  this->_outputs.insert(fnew);
  this->wr_interactions.insert(fnew);
}

uint64_t Command::descendants(void) {
  uint64_t ret = 0;
  for (auto c : getChildren()) {
    ret += 1 + c->descendants();
  }
  return ret;
}

void Command::collapse(std::set<Command*>* commands) {
  // std::cerr << "Collapsing set of size " << commands->size() << std::endl;
  unsigned int ansc_depth = _depth;
  // find the minimum common depth
  for (auto c : *commands) {
    if (c->_depth < ansc_depth) {
      ansc_depth = c->_depth;
    }
  }
  bool fully_collapsed = false;
  while (!fully_collapsed) {
    // std::cerr << "  Target depth " << ansc_depth << std::endl;
    // collapse all commands to this depth
    Command* prev_command = nullptr;
    Command* cur_command;
    fully_collapsed = true;
    for (auto c : *commands) {
      cur_command = c->collapse_helper(ansc_depth);
      // std::cerr << "    " << std::string(c->cmd.asPtr().asChars().begin(), c->cmd.asPtr().size())
      // << " collapsed to " << std::string(cur_command->cmd.asPtr().asChars().begin(),
      // cur_command->cmd.asPtr().size()) << std::endl;
      if (cur_command != prev_command && prev_command != nullptr) {
        fully_collapsed = false;
      }
      prev_command = cur_command;
    }
    ansc_depth--;
  }
}

Command* Command::collapse_helper(unsigned int min_depth) {
  if (_depth > min_depth) {
    this->collapse_with_parent = true;
    return _parent->collapse_helper(min_depth);
  } else {
    return this;
  }
}

bool Command::canDependOn(const File* f) {
  // If this command is the only writer, it cannot depend on the file

  if (f->getWriter() == this) return false;

  // If the file is not written and was created by this command, it cannot depend on the file
  if (!f->isWritten() && f->getCreator() == this) return false;

  // Otherwise the command can depend on the file
  return true;
}
