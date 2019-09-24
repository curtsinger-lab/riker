#include "core/BuildGraph.hh"

#include <utility>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "core/Process.hh"
#include "core/Serializer.hh"

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
  Serializer serializer;

  // Prepare files for serialization: we've already fingerprinted the old versions,
  // but we need to fingerprint the latest versions
  for (std::shared_ptr<File> f : _latest_versions) {
    f->fingerprint();
  }

  // Add files to the serializer
  for (std::shared_ptr<File> f : _files) {
    // Files can check whether or not they should be saved
    // Also skip the phony files created for stdin, stdout, and stderr
    if (f->shouldSave() && f->getPath() != "<<stdint>>" && f->getPath() != "<<stdout>>" &&
        f->getPath() != "<<stderr>>") {
      serializer.addFile(f);
    }
  }

  // Add the root command (and its descendants) to the serializer
  serializer.addCommand(_root_command);

  // Run the serialization
  serializer.serialize("db.dodo");
}
