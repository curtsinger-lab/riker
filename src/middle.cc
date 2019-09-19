#include "middle.h"

#include <iostream>

#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <capnp/message.h>
#include <capnp/serialize.h>

#include "db.capnp.h"

#include "file.hh"

/* ------------------------------ Command Methods -----------------------------------------*/
void Command::add_input(File* f) {
  // search through all files, do versioning
  f->addInteractor(this);
  f->addReader(this);
  this->inputs.insert(f);

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

void Command::add_output(File* f, size_t file_location) {
  // if we've written to the file before, check for a race
  if (!f->isPipe()) {
    for (auto wr : this->wr_interactions) {
      if (f->getLocation() == wr->getLocation()) {
        this->outputs.insert(f);
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
  this->outputs.insert(fnew);
  this->wr_interactions.insert(fnew);
}

uint64_t Command::descendants(void) {
  uint64_t ret = 0;
  for (auto c : this->children) {
    ret += 1 + c->descendants();
  }
  return ret;
}

static uint64_t serialize_commands(Command* command,
                                   ::capnp::List<db::Command>::Builder& command_list,
                                   uint64_t start_index, std::map<Command*, uint64_t>& command_ids,
                                   std::map<File*, uint64_t>& file_ids) {
  command_ids[command] = start_index;
  command_list[start_index].setExecutable(command->getCommand());
  command_list[start_index].setOutOfDate(false);
  command_list[start_index].setCollapseWithParent(command->collapse_with_parent);
  auto argv = command_list[start_index].initArgv(command->getArguments().size());
  uint64_t argv_index = 0;
  for (auto& arg : command->getArguments()) {
    argv.set(argv_index, arg);
    argv_index++;
  }
  auto initial_fds = command_list[start_index].initInitialFDs(command->initial_fds.size());
  size_t fd_index = 0;
  for (auto fd = command->initial_fds.begin(); fd != command->initial_fds.end(); ++fd) {
    auto file_id = file_ids.find(fd->second.file);
    if (file_id == file_ids.end()) {
      continue;
    }

    initial_fds[fd_index].setFd(fd->first);
    initial_fds[fd_index].setFileID(file_id->second);
    initial_fds[fd_index].setCanRead(fd->second.access_mode != O_WRONLY);
    initial_fds[fd_index].setCanWrite(fd->second.access_mode != O_RDONLY);
    fd_index++;
  }

  uint64_t index = start_index + 1;
  for (auto c : command->children) {
    index = serialize_commands(c, command_list, index, command_ids, file_ids);
  }

  command_list[start_index].setDescendants(index - start_index - 1);
  return index;
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

/* -------------------------- FileDescriptor Methods --------------------------------------*/
FileDescriptor::FileDescriptor() {}
FileDescriptor::FileDescriptor(size_t location_index, int access_mode, bool cloexec) :
    location_index(location_index),
    access_mode(access_mode),
    cloexec(cloexec) {}

void Process::exec(Trace& trace, std::string exe_path) {
  _command = _command->createChild(exe_path);

  // Close all cloexec file descriptors
  for (auto fd_entry = fds.begin(); fd_entry != fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }

  // Close all mmaps, since the address space is replaced
  for (auto file : mmaps) {
    file->removeMmap(this);
  }

  // Mark the initial open file descriptors
  for (auto it = fds.begin(); it != fds.end(); ++it) {
    it->second.file = trace.latest_versions[it->second.location_index];
  }
  _command->initial_fds = fds;

  // Assume that we can at any time write to stdout or stderr
  // TODO: Instead of checking whether we know about stdout and stderr,
  // tread the initial stdout and stderr properly as pipes
  if (fds.find(fileno(stdout)) != fds.end()) {
    trace.add_mmap(this->thread_id, fileno(stdout));
  }
  if (fds.find(fileno(stderr)) != fds.end()) {
    trace.add_mmap(this->thread_id, fileno(stderr));
  }
}

/* -------------------------- Trace_State Methods ----------------------------------------*/

void Trace::serialize_graph(void) {
  ::capnp::MallocMessageBuilder message;

  db::Graph::Builder graph = message.initRoot<db::Graph>();

  // Prepare files for serialization: we've already fingerprinted the old versions,
  // but we need to fingerprint the latest versions
  for (File* f : latest_versions) {
    f->fingerprint();
    f->setLatestVersion();
  }

  // Serialize files
  std::map<File*, uint64_t> file_ids;
  uint64_t file_count = 0;
  uint64_t input_count = 0;
  uint64_t output_count = 0;
  uint64_t create_count = 0;
  uint64_t modify_count = 0;
  for (auto file : this->files) {
    // We only care about files that are either written or read so filter those out.
    if (file->shouldSave()) {
      file_ids[file] = file_count;
      input_count += file->getReaders().size();
      if (file->isWritten()) output_count++;
      if (file->isCreated()) create_count++;
      modify_count += (!file->isCreated() && file->getPreviousVersion() != nullptr &&
                       (file->getPreviousVersion()->isCreated() ||
                        (file->getPreviousVersion()->getPreviousVersion() != nullptr &&
                         !file->getPreviousVersion()->isRemoved())));
      file_count += 1;
    }
  }
  auto files = graph.initFiles(file_count);
  for (auto file_entry : file_ids) {
    auto file = file_entry.first;
    auto file_id = file_entry.second;
    // TODO: Use orphans and avoid copying?
    file->serialize(files[file_id]);
  }

  // Serialize commands
  std::map<Command*, uint64_t> command_ids;
  uint64_t command_count = 0;
  for (auto c : this->commands) {
    command_count += 1 + c->descendants();
  }
  auto commands = graph.initCommands(command_count);
  uint64_t command_index = 0;
  for (auto c : this->commands) {
    command_index = serialize_commands(c, commands, command_index, command_ids, file_ids);
  }

  // Serialize dependencies
  auto inputs = graph.initInputs(input_count);
  auto outputs = graph.initOutputs(output_count);
  auto creations = graph.initCreations(create_count);
  auto modifications = graph.initModifications(modify_count);
  uint64_t input_index = 0;
  uint64_t output_index = 0;
  uint64_t create_index = 0;
  uint64_t modify_index = 0;
  for (auto file_entry : file_ids) {
    auto file = file_entry.first;
    auto file_id = file_entry.second;
    for (auto user : file->getReaders()) {
      uint64_t user_id = command_ids[user];
      inputs[input_index].setInputID(file_id);
      inputs[input_index].setOutputID(user_id);
      input_index++;
    }
    if (file->isWritten()) {
      uint64_t writer_id = command_ids[file->getWriter()];
      outputs[output_index].setInputID(writer_id);
      outputs[output_index].setOutputID(file_id);
      output_index++;
    }
    if (file->isCreated()) {
      uint64_t creator_id = command_ids[file->getCreator()];
      creations[create_index].setInputID(creator_id);
      creations[create_index].setOutputID(file_id);
      create_index++;
    }
    if (!file->isCreated() && file->getPreviousVersion() != nullptr &&
        (file->getPreviousVersion()->isCreated() ||
         (file->getPreviousVersion()->getPreviousVersion() != nullptr &&
          !file->getPreviousVersion()->isRemoved()))) {
      uint64_t prev_id = file_ids[file->getPreviousVersion()];
      modifications[modify_index].setInputID(prev_id);
      modifications[modify_index].setOutputID(file_id);
      modify_index++;
    }
  }

  // Serialize removal edges
  uint64_t removal_count = 0;
  for (auto c : command_ids) {
    for (auto f : c.first->deleted_files) {
      auto file_id = file_ids.find(f);
      if (file_id != file_ids.end()) {
        removal_count++;
      }
    }
  }
  auto removals = graph.initRemovals(removal_count);
  uint64_t removal_index = 0;
  for (auto c : command_ids) {
    for (auto f : c.first->deleted_files) {
      auto file_id = file_ids.find(f);
      if (file_id != file_ids.end()) {
        removals[removal_index].setInputID(c.second);
        removals[removal_index].setOutputID(file_ids[f]);
        removal_index++;
      }
    }
  }

  // TODO: Is kj::OutputStream suitable?
  int db_file = open("db.dodo", O_CREAT | O_TRUNC | O_WRONLY,
                     S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if (db_file < 0) {
    perror("Failed to open database");
    exit(2);
  }
  writeMessageToFd(db_file, message);
}
