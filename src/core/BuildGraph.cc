#include "core/BuildGraph.hh"

#include <utility>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "core/Process.hh"
#include "db/Serializer.hh"

BuildGraph::BuildGraph(std::string starting_dir) : _starting_dir(starting_dir) {
  size_t stdin_location = _latest_versions.size();
  _stdin = std::make_shared<File>(*this, stdin_location, db::FileType::PIPE, "<<stdin>>");
  _files.push_front(_stdin);
  _latest_versions.push_back(_stdin);

  size_t stdout_location = _latest_versions.size();
  _stdout = std::make_shared<File>(*this, stdout_location, db::FileType::PIPE, "<<stdout>>");
  _files.push_front(_stdout);
  _latest_versions.push_back(_stdout);

  size_t stderr_location = _latest_versions.size();
  _stderr = std::make_shared<File>(*this, stderr_location, db::FileType::PIPE, "<<stderr>>");
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
  std::shared_ptr<File> new_node =
      std::make_shared<File>(*this, location, db::FileType::REGULAR, path, nullptr);
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
  
  auto location = findFile(executable);
  auto f = _latest_versions[location]->getLatestVersion();
  
  _processes[pid]->traceRead(f);
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
  std::shared_ptr<File> f =
      std::make_shared<File>(*this, location, db::FileType::PIPE, "", proc->getCommand());
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
  if (file.fd == AT_FDCWD) {
    size_t file_location = findFile(file.path);
    std::shared_ptr<File> f = _latest_versions[file_location];
    _processes[pid]->traceRead(f);

  } else {
    _processes[pid]->traceRead(file.fd);
  }
}

void BuildGraph::traceModify(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    size_t file_location = findFile(file.path);
    std::shared_ptr<File> f = _latest_versions[file_location];
    _processes[pid]->traceModify(f);

  } else {
    _processes[pid]->traceModify(file.fd);
  }
}

void BuildGraph::traceCreate(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    size_t file_location = findFile(file.path);
    std::shared_ptr<File> f = _latest_versions[file_location];
    _processes[pid]->traceCreate(f);

  } else {
    _processes[pid]->traceCreate(file.fd);
  }
}

void BuildGraph::traceRemove(pid_t pid, struct file_reference& file) {
  if (file.fd == AT_FDCWD) {
    size_t file_location = findFile(file.path);
    std::shared_ptr<File> f = _latest_versions[file_location];
    _processes[pid]->traceRemove(f);

  } else {
    _processes[pid]->traceRemove(file.fd);
  }
}

void BuildGraph::serialize(Serializer& serializer) {
  // Prepare files for serialization: we've already fingerprinted the old versions,
  // but we need to fingerprint the latest versions
  for (std::shared_ptr<File> f : _latest_versions) {
    f->fingerprint();
  }

  // Add files to the serializer
  for (std::shared_ptr<File> f : _files) {
    // Files can check whether or not they should be saved
    // Also skip the phony files created for stdin, stdout, and stderr
    if (f->shouldSave() && f->getPath() != "<<stdin>>" && f->getPath() != "<<stdout>>" &&
        f->getPath() != "<<stderr>>") {
      serializer.addFile(f);
    }
  }

  // Add the root command (and its descendants) to the serializer
  serializer.addCommand(_root_command);

  // Run the serialization
  serializer.serialize();
}
