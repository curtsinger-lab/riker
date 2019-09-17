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
#include <iostream>

#include "db.capnp.h"
#include "fingerprint.h"
#include "middle.h"

/* ------------------------------ Command Methods -----------------------------------------*/
void Command::add_input(File* f) {
  // search through all files, do versioning
  f->interactions.push_front(this);
  f->users.insert(this);
  this->inputs.insert(f);

  // if we've read from the file previously, check for a race
  if (f->serialized.getReader().getType() == db::FileType::PIPE) {
    return;
  }

  for (auto rd : this->rd_interactions) {
    if (f->location == rd->location && f->writer != this) {
      if (f->version == rd->version) {
        return;
      } else /* we've found a race */ {
        std::set<Command*> conflicts = f->collapse(rd->version);
        this->collapse(&conflicts);
      }
    }
  }
  // mark that we've now interacted with this version of the file
  this->rd_interactions.insert(f);
}

void Command::add_output(File* f, size_t file_location) {
  // if we've written to the file before, check for a race
  if (f->serialized.getReader().getType() != db::FileType::PIPE) {
    for (auto wr : this->wr_interactions) {
      if (f->location == wr->location) {
        this->outputs.insert(f);
        if (f->version == wr->version) {
          return;
        } else {
          std::set<Command*> conflicts = f->collapse(wr->version);
          this->collapse(&conflicts);
          // wr->has_race = true;
          // this->has_race = true;
          return;
        }
      }
    }
  }

  f->interactions.push_front(this);
  // if we haven't written to this file before, create a new version
  File* fnew;
  if ((f->creator != nullptr && f->writer == nullptr) || f->writer == this) {
    // Unless we just created it, in which case it is pristine
    fnew = f;
  } else {
    fnew = f->make_version();
    fnew->creator = nullptr;
  }
  fnew->writer = this;
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
                                   uint64_t start_index,
                                   std::map<Command*, uint64_t>& command_ids,
                                   std::map<File*, uint64_t>& file_ids) {
  command_ids[command] = start_index;
  command_list[start_index].setExecutable(stringToBlob(command->getCommand()));
  command_list[start_index].setOutOfDate(false);
  command_list[start_index].setCollapseWithParent(command->collapse_with_parent);
  auto argv = command_list[start_index].initArgv(command->getArguments().size());
  uint64_t argv_index = 0;
  for (auto& arg : command->getArguments()) {
    argv.set(argv_index, stringToBlob(arg));
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

/* ------------------------------- File Methods -------------------------------------------*/
File::File(size_t location, bool is_pipe, BlobPtr path, Command* creator,
                   Trace* state, File* prev_version) :
    location(location),
    serialized(state->temp_message.getOrphanage().newOrphan<db::File>()),
    creator(creator),
    writer(nullptr),
    state(state),
    prev_version(prev_version),
    version(0),
    known_removed(false) {
  // TODO: consider using orphans to avoid copying
  if (is_pipe) {
    this->serialized.get().setType(db::FileType::PIPE);
  } else {
    this->serialized.get().setType(db::FileType::REGULAR);
    this->serialized.get().setPath(path);
  }
}

// return a set of the commands which raced on this file, back to the parameter version
std::set<Command*> File::collapse(unsigned int version) {
  // this->has_race = true;
  File* cur_file = this;
  std::set<Command*> conflicts;
  while (cur_file->version != version) {
    // add writer and all readers to conflict set
    if (cur_file->writer != nullptr) {
      conflicts.insert(cur_file->writer);
    }
    for (auto rd : cur_file->interactions) {
      conflicts.insert(rd);
    }
    // add all mmaps to conflicts
    for (auto m : cur_file->mmaps) {
      conflicts.insert(m->getCommand());
    }
    // step back a version
    cur_file = cur_file->prev_version;
  }
  return conflicts;
}

// file can only be depended on if it wasn't truncated/created, and if the current command isn't
// the only writer
bool File::can_depend(Command* cmd) {
  return this->writer != cmd && (this->writer != nullptr || this->creator != cmd);
}

File* File::make_version(void) {
  // We are at the end of the current version, so snapshot with a fingerprint
  set_fingerprint(this->serialized.get(), true);
  this->serialized.get().setLatestVersion(false);

  File* f =
      new File(this->location, this->serialized.getReader().getType() == db::FileType::PIPE,
                   this->serialized.getReader().getPath(), this->creator, this->state, this);
  f->version = this->version + 1;

  this->state->files.insert(f);
  this->state->latest_versions[this->location] = f;
  return f;
}

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
  for (auto f = mmaps.begin(); f != mmaps.end(); ++f) {
    (*f)->mmaps.erase(this);
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
  for (size_t location = 0; location < this->latest_versions.size(); location++) {
    set_fingerprint(this->latest_versions[location]->serialized.get(),
                    !this->latest_versions[location]->users.empty());
    this->latest_versions[location]->serialized.get().setLatestVersion(true);
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
    if (!file->users.empty() || file->writer != nullptr || file->creator != nullptr ||
        (file->prev_version != nullptr && !file->known_removed)) {
      file_ids[file] = file_count;
      input_count += file->users.size();
      output_count += (file->writer != nullptr);
      create_count += (file->creator != nullptr);
      modify_count +=
          (file->creator == nullptr && file->prev_version != nullptr &&
           (file->prev_version->creator != nullptr ||
            (file->prev_version->prev_version != nullptr && !file->prev_version->known_removed)));
      file_count += 1;
    }
  }
  auto files = graph.initFiles(file_count);
  for (auto file_entry : file_ids) {
    auto file = file_entry.first;
    auto file_id = file_entry.second;
    // TODO: Use orphans and avoid copying?
    files.setWithCaveats(file_id, file->serialized.getReader());
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
    for (auto user : file->users) {
      uint64_t user_id = command_ids[user];
      inputs[input_index].setInputID(file_id);
      inputs[input_index].setOutputID(user_id);
      input_index++;
    }
    if (file->writer != nullptr) {
      uint64_t writer_id = command_ids[file->writer];
      outputs[output_index].setInputID(writer_id);
      outputs[output_index].setOutputID(file_id);
      output_index++;
    }
    if (file->creator != nullptr) {
      uint64_t creator_id = command_ids[file->creator];
      creations[create_index].setInputID(creator_id);
      creations[create_index].setOutputID(file_id);
      create_index++;
    }
    if (file->creator == nullptr && file->prev_version != nullptr &&
        (file->prev_version->creator != nullptr ||
         (file->prev_version->prev_version != nullptr && !file->prev_version->known_removed))) {
      uint64_t prev_id = file_ids[file->prev_version];
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
