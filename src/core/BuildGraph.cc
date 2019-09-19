#include "core/BuildGraph.hh"

#include <cstdint>
#include <set>
#include <utility>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <capnp/list.h>
#include <capnp/message.h>
#include <capnp/serialize.h>
#include <kj/string.h>

#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "core/Process.hh"
#include "db/db.capnp.h"

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

void BuildGraph::newProcess(pid_t pid, Command* cmd) {
  Process* proc = new Process(pid, _starting_dir, cmd);
  _processes.emplace(pid, proc);
}

size_t BuildGraph::find_file(std::string path) {
  for (size_t index = 0; index < this->latest_versions.size(); index++) {
    if (!this->latest_versions[index]->isPipe() &&
        this->latest_versions[index]->getPath() == path) {
      return index;
    }
  }
  size_t location = this->latest_versions.size();
  File& new_node = files.emplace_front(*this, location, false, path, nullptr, nullptr);
  this->latest_versions.push_back(&new_node);
  return location;
}

void BuildGraph::add_chdir(pid_t pid, struct file_reference& file) {
  _processes[pid]->chdir(file.path);
}

void BuildGraph::add_chroot(pid_t pid, struct file_reference& file) {
  _processes[pid]->chroot(file.path);
}

void BuildGraph::add_fork(pid_t pid, pid_t child_pid) {
  // fprintf(stdout, "[%d] Fork %d\n", parent_proc->thread_id,
  // child_process_id);
  _processes[child_pid] = _processes[pid]->fork(child_pid);
}

void BuildGraph::add_clone(pid_t pid, pid_t thread_id) { _processes[thread_id] = _processes[pid]; }

void BuildGraph::add_exec(pid_t pid, std::string exe_path, std::list<std::string> arguments) {
  _processes[pid]->exec(*this, exe_path);
  for (auto arg : arguments) {
    _processes[pid]->getCommand()->addArgument(arg);
  }
}

void BuildGraph::add_exit(pid_t pid) {
  auto proc = _processes[pid];
  for (auto f : proc->mmaps) {
    f->removeMmap(proc);
  }
}

void BuildGraph::add_open(pid_t pid, int fd, struct file_reference& file, int access_mode,
                          bool is_rewrite, bool cloexec, mode_t mode) {
  auto proc = _processes[pid];

  // fprintf(stdout, "[%d] Open %d -> ", proc->thread_id, fd);
  // TODO take into account root and cwd
  size_t file_location = this->find_file(file.path);
  File* f = this->latest_versions[file_location];
  if (is_rewrite && (f->getCreator() != proc->getCommand() || f->isWritten())) {
    // fprintf(stdout, "REWRITE ");
    f = f->createVersion();
    f->setCreator(proc->getCommand());
    f->setWriter(nullptr);
    f->setMode(mode);
  }
  proc->fds[fd] = FileDescriptor(file_location, access_mode, cloexec);
  if (file.fd == AT_FDCWD) {
    // fprintf(stdout, " %.*s\n", (int)file.path.size(),
    // file.path.asChars().begin());
  } else {
    // fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(),
    // file.path.asChars().begin());
  }
}

void BuildGraph::add_close(pid_t pid, int fd) { _processes[pid]->fds.erase(fd); }

void BuildGraph::add_pipe(pid_t pid, int fds[2], bool cloexec) {
  auto proc = _processes[pid];
  // fprintf(stdout, "[%d] Pipe %d, %d\n", proc->thread_id, fds[0], fds[1]);
  size_t location = this->latest_versions.size();
  File& p = files.emplace_front(*this, location, true, "", proc->getCommand(), nullptr);
  latest_versions.push_back(&p);
  proc->fds[fds[0]] = FileDescriptor(location, O_RDONLY, cloexec);
  proc->fds[fds[1]] = FileDescriptor(location, O_WRONLY, cloexec);
}

void BuildGraph::add_dup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
  auto proc = _processes[pid];
  auto duped_file = proc->fds.find(duped_fd);
  if (duped_file == proc->fds.end()) {
    proc->fds.erase(new_fd);
  } else {
    proc->fds[new_fd] =
        FileDescriptor(duped_file->second.location_index, duped_file->second.access_mode, cloexec);
  }
}

void BuildGraph::add_set_cloexec(pid_t pid, int fd, bool cloexec) {
  auto proc = _processes[pid];
  auto file = proc->fds.find(fd);
  if (file != proc->fds.end()) {
    file->second.cloexec = cloexec;
  }
}

void BuildGraph::add_mmap(pid_t pid, int fd) {
  auto proc = _processes[pid];
  FileDescriptor& desc = proc->fds.find(fd)->second;
  File* f = this->latest_versions[desc.location_index];
  f->addMmap(proc);
  proc->mmaps.insert(f);
  // std::cout << "MMAP ";
  if (desc.access_mode != O_WRONLY) {
    // std::cout << "read ";
    proc->getCommand()->add_input(f);
  }
  if (desc.access_mode != O_RDONLY) {
    // std::cout << "write";
    proc->getCommand()->add_output(f, desc.location_index);
  }
}

void BuildGraph::add_dependency(pid_t pid, struct file_reference& file, enum dependency_type type) {
  auto proc = _processes[pid];
  size_t file_location;
  if (file.fd == AT_FDCWD) {
    file_location = this->find_file(file.path);
  } else {
    // fprintf(stdout, "[%d] Dep: %d -> ", proc->thread_id, file.fd);
    if (proc->fds.find(file.fd) == proc->fds.end()) {
      // TODO: Use a proper placeholder and stop fiddling with string
      // manipulation
      std::string path_str;
      switch (file.fd) {  // This is a temporary hack until we handle pipes well
        case 0:
          path_str = "<<stdin>>";
          break;
        case 1:
          path_str = "<<stdout>>";
          break;
        case 2:
          path_str = "<<stderr>>";
          break;
        default:
          path_str = "file not found, fd: " + std::to_string(file.fd);
          break;
      }
      file_location = this->find_file(path_str);
    } else {
      file_location = proc->fds.find(file.fd)->second.location_index;
    }
  }
  File* f = this->latest_versions[file_location];

  // fprintf(stdout, "file: %.*s-%d ", (int)path.size(),
  // path.asChars().begin(), f->version);
  switch (type) {
    case DEP_READ:
      // fprintf(stdout, "read");
      if (proc->getCommand()->canDependOn(f)) {
        proc->getCommand()->add_input(f);
        // fprintf(stdout, ", depend");
      }
      break;
    case DEP_MODIFY:
      // fprintf(stdout, "modify");
      proc->getCommand()->add_output(f, file_location);
      break;
    case DEP_CREATE:
      // fprintf(stdout, "create");
      // Creation means creation only if the file does not already exist.
      if (f->isCreated() && !f->isWritten()) {
        bool file_exists;
        if (f->isRemoved() || f->isPipe()) {
          file_exists = false;
        } else {
          struct stat stat_info;
          file_exists = (lstat(f->getPath().cStr(), &stat_info) == 0);
        }

        if (!file_exists) {
          f->setCreator(proc->getCommand());
          f->setRemoved(false);
        }
      }
      break;
    case DEP_REMOVE:
      // fprintf(stdout, "remove");
      proc->getCommand()->deleted_files.insert(f);
      f = f->createVersion();
      f->setCreator(nullptr);
      f->setWriter(nullptr);
      f->setRemoved();
      break;
  }
}

void BuildGraph::serialize_graph(void) {
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
  for (auto& f : this->files) {
    File* file = &f;
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
