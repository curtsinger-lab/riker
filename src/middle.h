#pragma once

#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <capnp/message.h>
#include <capnp/orphan.h>
#include <kj/array.h>

#include "db.capnp.h"

#include "util.hh"

typedef kj::Array<kj::byte> Blob;
typedef kj::ArrayPtr<const kj::byte> BlobPtr;

enum dependency_type { DEP_READ, DEP_MODIFY, DEP_CREATE, DEP_REMOVE };

struct file_reference {
  // fd may be AT_FDCWD and path may be NULL, but not both. If both are present,
  // then path is relative with respect to the directory in fd.
  int fd;
  Blob path;
  // Whether to follow the link if the reference points at a symlink
  bool follow_links;
};

struct new_file;
struct FileDescriptor;
struct Process;
struct Trace;

struct new_command {
  Trace& state;
  Blob cmd;
  std::list<new_command*> children;
  std::set<new_file*> inputs;
  std::set<new_file*> outputs;
  std::set<new_file*> wr_interactions;
  std::set<new_file*> rd_interactions;
  std::set<new_file*> deleted_files;
  std::list<Blob> args;
  new_command* parent;
  unsigned int depth;
  bool collapse_with_parent;
  std::map<int, FileDescriptor> initial_fds;

  new_command(Trace& state, Blob&& args, new_command* parent, unsigned int depth);
  void add_input(new_file* f);
  void add_output(new_file* f, size_t file_location);
  size_t descendants(void);
  new_command* collapse_helper(unsigned int depth);
  void collapse(std::set<new_command*>* commands);
};

struct new_file {
  size_t location;
  capnp::Orphan<db::File> serialized;
  std::set<new_command*> users;
  std::set<Process*> mmaps;
  std::list<new_command*> interactions;
  std::list<new_command*> conflicts;
  new_command* creator;
  new_command* writer;
  Trace* state;
  new_file* prev_version;
  unsigned int version;
  bool known_removed;

  new_file(size_t location, bool is_pipe, BlobPtr path, new_command* creator, Trace* state,
           new_file* prev_version);
  std::set<new_command*> collapse(unsigned int depth);
  bool can_depend(new_command* cmd);
  new_file* make_version(void);
};

struct FileDescriptor {
  size_t location_index;  // Used in Process::fds
  new_file* file;         // Used in new_command::initial_fds
  int access_mode;
  bool cloexec;

  FileDescriptor();
  FileDescriptor(size_t location_index, int access_mode, bool cloexec);
};

struct Process {
  pid_t thread_id;
  std::map<int, FileDescriptor> fds;
  std::set<new_file*> mmaps;

  Process(pid_t thread_id, std::string cwd, new_command* command) :
      thread_id(thread_id),
      _command(command),
      _cwd(cwd) {}

  new_command* getCommand() { return _command; }

  void chdir(std::string newdir) { _cwd = newdir; }

  void chroot(std::string newroot) { _root = newroot; }

  Process* fork(pid_t child_pid) {
    Process* child_proc = new Process(child_pid, _cwd, _command);
    child_proc->fds = fds;
    return child_proc;
  }

  void exec(Trace& trace, Blob&& exe_path);

 private:
  new_command* _command;
  std::string _cwd;
  std::string _root;
};

struct file_comparator {
  bool operator()(new_file* const& lhs, new_file* const& rhs) const {
    //  return false;

    auto lhs_reader = lhs->serialized.getReader();
    auto rhs_reader = rhs->serialized.getReader();
    if (lhs_reader.getType() == db::FileType::PIPE) {
      return true;
    } else if (rhs_reader.getType() == db::FileType::PIPE) {
      return false;
    }

    auto path1 = lhs_reader.getPath();
    auto path2 = rhs_reader.getPath();

    // compare to find alphabetic order
    for (size_t i = 0; i < std::min(path1.size(), path2.size()); i++) {
      if (path1[i] != path2[i]) {
        return path1[i] < path2[i];
      }
    }

    // if the first min(path1.size(), path2(size()) characters are the same,
    // order the shorter string first
    // return true;
    return path1.size() <= path2.size();
  }
};

struct Trace {
  std::set<new_file*, file_comparator> files;
  std::vector<new_file*> latest_versions;
  std::list<new_command*> commands;
  ::capnp::MallocMessageBuilder temp_message;

  Trace(std::string starting_dir) : _starting_dir(starting_dir) {}

  Blob getStartingDir() { return stringToBlob(_starting_dir); }

  void newProcess(pid_t pid, new_command* cmd) {
    Process* proc = new Process(pid, _starting_dir, cmd);
    _processes.emplace(pid, proc);
    // processes.insert(std::pair<pid_t, Process*>(pid, proc));
  }

  size_t find_file(BlobPtr path) {
    for (size_t index = 0; index < this->latest_versions.size(); index++) {
      if (this->latest_versions[index]->serialized.getReader().getType() != db::FileType::PIPE &&
          this->latest_versions[index]->serialized.getReader().getPath() == path) {
        return index;
      }
    }
    size_t location = this->latest_versions.size();
    new_file* new_node = new new_file(location, false, kj::heapArray(path), nullptr, this, nullptr);
    this->files.insert(new_node);
    this->latest_versions.push_back(new_node);
    return location;
  }

  void serialize_graph();

  void add_dependency(pid_t pid, struct file_reference& file, enum dependency_type type) {
    Process* proc = _processes[pid];
    size_t file_location;
    if (file.fd == AT_FDCWD) {
      file_location = this->find_file(file.path.asPtr());
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
        Blob path_buf = kj::heapArray((const kj::byte*)path_str.data(), path_str.size());
        file_location = this->find_file(path_buf.asPtr());
      } else {
        file_location = proc->fds.find(file.fd)->second.location_index;
      }
    }
    new_file* f = this->latest_versions[file_location];

    // fprintf(stdout, "file: %.*s-%d ", (int)path.size(),
    // path.asChars().begin(), f->version);
    switch (type) {
      case DEP_READ:
        // fprintf(stdout, "read");
        if (f->can_depend(proc->getCommand())) {
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
        if (f->creator == nullptr && f->writer == nullptr) {
          bool file_exists;
          if (f->known_removed || f->serialized.getReader().getType() == db::FileType::PIPE) {
            file_exists = false;
          } else {
            struct stat stat_info;
            auto path_string = std::string(f->serialized.getReader().getPath().asChars().begin(),
                                           f->serialized.getReader().getPath().size());
            file_exists = (lstat(path_string.c_str(), &stat_info) == 0);
          }

          if (!file_exists) {
            f->creator = proc->getCommand();
            f->known_removed = false;
          }
        }
        break;
      case DEP_REMOVE:
        // fprintf(stdout, "remove");
        proc->getCommand()->deleted_files.insert(f);
        f = f->make_version();
        f->creator = nullptr;
        f->writer = nullptr;
        f->known_removed = true;
        break;
    }
    if (!file.follow_links) {
      // fprintf(stdout, " (nofollow)");
    }
    if (file.path == nullptr) {
      // fprintf(stdout, " FD %d\n", file.fd);
    } else if (file.fd == AT_FDCWD) {
      // fprintf(stdout, " %.*s\n", (int)file.path.size(),
      // file.path.asChars().begin());
    } else {
      // fprintf(stdout, " {%d/}%.*s\n", file.fd, (int)file.path.size(),
      // file.path.asChars().begin());
    }
  }

  void add_change_cwd(pid_t pid, struct file_reference& file) {
    _processes[pid]->chdir(blobToString(file.path));
  }

  void add_change_root(pid_t pid, struct file_reference& file) {
    _processes[pid]->chroot(blobToString(file.path));
  }

  void add_open(pid_t pid, int fd, struct file_reference& file, int access_mode, bool is_rewrite,
                bool cloexec, mode_t mode) {
    Process* proc = _processes[pid];

    // fprintf(stdout, "[%d] Open %d -> ", proc->thread_id, fd);
    // TODO take into account root and cwd
    size_t file_location = this->find_file(file.path.asPtr());
    new_file* f = this->latest_versions[file_location];
    if (is_rewrite && (f->creator != proc->getCommand() || f->writer != nullptr)) {
      // fprintf(stdout, "REWRITE ");
      f = f->make_version();
      f->creator = proc->getCommand();
      f->writer = nullptr;
      f->serialized.get().setMode(mode);
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

  void add_pipe(pid_t pid, int fds[2], bool cloexec) {
    Process* proc = _processes[pid];
    // fprintf(stdout, "[%d] Pipe %d, %d\n", proc->thread_id, fds[0], fds[1]);
    size_t location = this->latest_versions.size();
    new_file* p = new new_file(location, true, Blob(), proc->getCommand(), this, NULL);
    this->files.insert(p);
    this->latest_versions.push_back(p);
    proc->fds[fds[0]] = FileDescriptor(location, O_RDONLY, cloexec);
    proc->fds[fds[1]] = FileDescriptor(location, O_WRONLY, cloexec);
  }

  void add_dup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
    Process* proc = _processes[pid];
    auto duped_file = proc->fds.find(duped_fd);
    if (duped_file == proc->fds.end()) {
      proc->fds.erase(new_fd);
    } else {
      proc->fds[new_fd] = FileDescriptor(duped_file->second.location_index,
                                         duped_file->second.access_mode, cloexec);
    }
  }

  void add_set_cloexec(pid_t pid, int fd, bool cloexec) {
    Process* proc = _processes[pid];
    auto file = proc->fds.find(fd);
    if (file != proc->fds.end()) {
      file->second.cloexec = cloexec;
    }
  }

  void add_mmap(pid_t pid, int fd) {
    Process* proc = _processes[pid];
    FileDescriptor& desc = proc->fds.find(fd)->second;
    new_file* f = this->latest_versions[desc.location_index];
    f->mmaps.insert(proc);
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

  void add_close(pid_t pid, int fd) { _processes[pid]->fds.erase(fd); }

  void add_fork(pid_t pid, pid_t child_pid) {
    // fprintf(stdout, "[%d] Fork %d\n", parent_proc->thread_id,
    // child_process_id);
    _processes[child_pid] = _processes[pid]->fork(child_pid);
  }

  void add_clone(pid_t pid, pid_t thread_id) { _processes[thread_id] = _processes[pid]; }

  void add_exec(pid_t pid, Blob&& exe_path) {
    Process* proc = _processes.at(pid);
    proc->exec(*this, std::move(exe_path));
  }

  void add_exec_argument(pid_t pid, Blob&& argument, int index) {
    _processes.at(pid)->getCommand()->args.push_back(std::move(argument));
  }

  void add_exit(pid_t pid) {
    Process* proc = _processes[pid];
    for (auto f : proc->mmaps) {
      f->mmaps.erase(proc);
    }
  }

 private:
  std::string _starting_dir;
  std::map<pid_t, Process*> _processes;
};
