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

#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "core/Process.hh"
#include "db/db.capnp.h"

static uint64_t serialize_commands(std::shared_ptr<Command> command,
                                   ::capnp::List<db::Command>::Builder& command_list,
                                   uint64_t start_index,
                                   std::map<std::shared_ptr<Command>, uint64_t>& command_ids,
                                   std::map<std::shared_ptr<File>, uint64_t>& file_ids) {
  command_ids[command] = start_index;
  command_list[start_index].setExecutable(command->getExecutable());
  command_list[start_index].setOutOfDate(false);
  command_list[start_index].setCollapseWithParent(command->getCollapseWithParent());
  auto argv = command_list[start_index].initArgv(command->getArguments().size());
  uint64_t argv_index = 0;
  for (auto& arg : command->getArguments()) {
    argv.set(argv_index, arg);
    argv_index++;
  }
  auto initial_fds = command_list[start_index].initInitialFDs(command->getInitialFDs().size());
  size_t fd_index = 0;

  for (auto fd : command->getInitialFDs()) {
    auto file_id = file_ids.find(fd.second.file);
    if (file_id == file_ids.end()) {
      continue;
    }

    initial_fds[fd_index].setFd(fd.first);
    initial_fds[fd_index].setFileID(file_id->second);
    initial_fds[fd_index].setCanRead(fd.second.access_mode != O_WRONLY);
    initial_fds[fd_index].setCanWrite(fd.second.access_mode != O_RDONLY);
    fd_index++;
  }

  uint64_t index = start_index + 1;
  for (auto c : command->getChildren()) {
    index = serialize_commands(c, command_list, index, command_ids, file_ids);
  }

  command_list[start_index].setDescendants(index - start_index - 1);
  return index;
}

BuildGraph::BuildGraph(std::string starting_dir) : _starting_dir(starting_dir) {
  size_t stdin_location = _latest_versions.size();
  _stdin = std::make_shared<File>(*this, stdin_location, true, "<<stdin>>");
  _files.push_front(_stdin);
  _latest_versions.push_back(_stdin);

  size_t stdout_location = _latest_versions.size();
  _stdout = std::make_shared<File>(*this, stdout_location, true, "<<stdout>>");
  _files.push_front(_stdout);
  _latest_versions.push_back(_stdout);

  size_t stderr_location = _latest_versions.size();
  _stderr = std::make_shared<File>(*this, stderr_location, true, "<<stderr>>");
  _files.push_front(_stderr);
  _latest_versions.push_back(_stderr);
}

void BuildGraph::newProcess(pid_t pid, std::shared_ptr<Command> cmd) {
  Process* proc = new Process(pid, _starting_dir, cmd);
  proc->setDefaultFds(_stdin, _stdout, _stderr);
  _processes.emplace(pid, proc);
}

size_t BuildGraph::findFile(std::string path) {
  for (size_t index = 0; index < _latest_versions.size(); index++) {
    if (!_latest_versions[index]->isPipe() && _latest_versions[index]->getPath() == path) {
      return index;
    }
  }
  size_t location = _latest_versions.size();
  std::shared_ptr<File> new_node = std::make_shared<File>(*this, location, false, path, nullptr);
  addFile(new_node);
  _latest_versions.push_back(new_node);
  return location;
}

void BuildGraph::traceChdir(pid_t pid, std::string path) {
  _processes[pid]->traceChdir(path);
}

void BuildGraph::traceChroot(pid_t pid, std::string path) {
  _processes[pid]->traceChroot(path);
}

void BuildGraph::traceFork(pid_t pid, pid_t child_pid) {
  _processes[child_pid] = _processes[pid]->traceFork(child_pid);
}

void BuildGraph::traceClone(pid_t pid, pid_t thread_id) {
  // Threads in the same process just appear as pid references to the same process
  _processes[thread_id] = _processes[pid];
}

void BuildGraph::traceExec(pid_t pid, std::string executable, const std::list<std::string>& args) {
  _processes[pid]->traceExec(*this, executable, args);
}

void BuildGraph::traceExit(pid_t pid) {
  _processes[pid]->traceExit();
}

void BuildGraph::traceOpen(pid_t pid, int fd, std::string path, int flags, mode_t mode) {
  size_t file_location = findFile(path);
  std::shared_ptr<File> f = _latest_versions[file_location];
  _processes[pid]->traceOpen(fd, f, flags, mode);
}

void BuildGraph::traceClose(pid_t pid, int fd) {
  _processes[pid]->traceClose(fd);
}

void BuildGraph::tracePipe(pid_t pid, int fds[2], bool cloexec) {
  auto proc = _processes[pid];

  size_t location = _latest_versions.size();
  std::shared_ptr<File> f = std::make_shared<File>(*this, location, true, "", proc->getCommand());
  addFile(f);
  _latest_versions.push_back(f);

  proc->tracePipe(fds[0], fds[1], f, cloexec);
}

void BuildGraph::traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
  _processes[pid]->traceDup(duped_fd, new_fd, cloexec);
}

void BuildGraph::traceSetCloexec(pid_t pid, int fd, bool cloexec) {
  _processes[pid]->traceSetCloexec(fd, cloexec);
}

void BuildGraph::traceMmap(pid_t pid, int fd) {
  _processes[pid]->traceMmap(*this, fd);
}

void BuildGraph::traceRead(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto fds = proc->getFds();

  size_t file_location;
  if (file.fd == AT_FDCWD) {
    file_location = findFile(file.path);
  } else {
    file_location = fds.find(file.fd)->second.location_index;
  }
  std::shared_ptr<File> f = _latest_versions[file_location];

  if (proc->getCommand()->canDependOn(f)) {
    proc->getCommand()->addInput(f);
  }
}

void BuildGraph::traceModify(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto fds = proc->getFds();

  size_t file_location;
  if (file.fd == AT_FDCWD) {
    file_location = findFile(file.path);
  } else {
    file_location = fds.find(file.fd)->second.location_index;
  }
  std::shared_ptr<File> f = _latest_versions[file_location];

  proc->getCommand()->addOutput(f);
}

void BuildGraph::traceCreate(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto fds = proc->getFds();

  size_t file_location;
  if (file.fd == AT_FDCWD) {
    file_location = findFile(file.path);
  } else {
    file_location = fds.find(file.fd)->second.location_index;
  }
  std::shared_ptr<File> f = _latest_versions[file_location];

  if (f->isCreated() && !f->isWritten()) {
    bool file_exists;
    if (f->isRemoved() || f->isPipe()) {
      file_exists = false;
    } else {
      struct stat stat_info;
      file_exists = (lstat(f->getPath().c_str(), &stat_info) == 0);
    }

    if (!file_exists) {
      f->setCreator(proc->getCommand());
      f->setRemoved(false);
    }
  }
}

void BuildGraph::traceRemove(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto fds = proc->getFds();

  size_t file_location;
  if (file.fd == AT_FDCWD) {
    file_location = findFile(file.path);
  } else {
    file_location = fds.find(file.fd)->second.location_index;
  }
  std::shared_ptr<File> f = _latest_versions[file_location];

  proc->getCommand()->addDeletedFile(f);
  f = f->createVersion();
  f->setCreator(nullptr);
  f->setWriter(nullptr);
  f->setRemoved();
}

void BuildGraph::serializeGraph() {
  ::capnp::MallocMessageBuilder message;

  db::Graph::Builder graph = message.initRoot<db::Graph>();

  // Prepare files for serialization: we've already fingerprinted the old versions,
  // but we need to fingerprint the latest versions
  for (std::shared_ptr<File> f : _latest_versions) {
    f->fingerprint();
  }

  // Serialize files
  std::map<std::shared_ptr<File>, uint64_t> file_ids;
  uint64_t file_count = 0;
  uint64_t input_count = 0;
  uint64_t output_count = 0;
  uint64_t create_count = 0;
  uint64_t modify_count = 0;
  for (std::shared_ptr<File> file : _files) {
    // We only care about files that are either written or read so filter those out.
    if (file->shouldSave()) {
      file_ids[file] = file_count;
      input_count += file->getReaders().size();
      if (file->isWritten()) output_count++;
      if (file->isCreated()) create_count++;
      if (file->isModified()) modify_count++;
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
  std::map<std::shared_ptr<Command>, uint64_t> command_ids;
  uint64_t command_count = 0;
  for (auto c : _commands) {
    command_count += 1 + c->numDescendants();
  }
  auto commands = graph.initCommands(command_count);
  uint64_t command_index = 0;
  for (auto c : _commands) {
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
    for (auto f : c.first->getDeletedFiles()) {
      auto file_id = file_ids.find(f);
      if (file_id != file_ids.end()) {
        removal_count++;
      }
    }
  }
  auto removals = graph.initRemovals(removal_count);
  uint64_t removal_index = 0;
  for (auto c : command_ids) {
    for (auto f : c.first->getDeletedFiles()) {
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
