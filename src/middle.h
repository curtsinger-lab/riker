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

struct File;
struct FileDescriptor;
struct Process;
struct Trace;

struct Command {
  Command(Trace& state, std::string cmd, Command* parent, unsigned int depth) :
      _state(state),
      _cmd(cmd),
      _parent(parent),
      _depth(depth) {}

  Command* createChild(std::string cmd) {
    Command* child = new Command(_state, cmd, this, _depth + 1);
    children.push_back(child);
    return child;
  }

  void add_input(File* f);
  void add_output(File* f, size_t file_location);
  size_t descendants(void);

  void collapse(std::set<Command*>* commands);

  Command* collapse_helper(unsigned int min_depth) {
    if (_depth > min_depth) {
      this->collapse_with_parent = true;
      return _parent->collapse_helper(min_depth);
    } else {
      return this;
    }
  }

  const std::string& getCommand() { return _cmd; }

  const std::vector<std::string>& getArguments() { return _args; }

  void addArgument(std::string arg) { _args.push_back(arg); }

 private:
  Trace& _state;
  std::string _cmd;
  std::vector<std::string> _args;
  Command* _parent;
  const unsigned int _depth;

 public:
  std::list<Command*> children;
  std::set<File*> inputs;
  std::set<File*> outputs;
  std::set<File*> wr_interactions;
  std::set<File*> rd_interactions;
  std::set<File*> deleted_files;
  bool collapse_with_parent;
  std::map<int, FileDescriptor> initial_fds;
};

struct File {
  File(size_t location, bool is_pipe, BlobPtr path, Command* creator, Trace* state,
       File* prev_version);
  std::set<Command*> collapse(unsigned int depth);
  bool can_depend(Command* cmd);
  File* make_version(void);
  
  std::string getPath() { return blobToString(_serialized.getReader().getPath()); }
  
  bool isPipe() { return _serialized.getReader().getType() == db::FileType::PIPE; }
  
  void setMode(uint16_t mode) { _serialized.get().setMode(mode); }
  
  void setLatestVersion() { _serialized.get().setLatestVersion(true); }
  
  db::File::Builder getBuilder() { return _serialized.get(); }
  
  db::File::Reader getReader() { return _serialized.getReader(); }

 public:
  size_t location;

 private:
  capnp::Orphan<db::File> _serialized;

 public:
  std::set<Command*> users;
  std::set<Process*> mmaps;
  std::list<Command*> interactions;
  std::list<Command*> conflicts;
  Command* creator;
  Command* writer;
  Trace* state;
  File* prev_version;
  unsigned int version;
  bool known_removed;
};

struct FileDescriptor {
  size_t location_index;  // Used in Process::fds
  File* file;             // Used in Command::initial_fds
  int access_mode;
  bool cloexec;

  FileDescriptor();
  FileDescriptor(size_t location_index, int access_mode, bool cloexec);
};

struct Process {
  pid_t thread_id;
  std::map<int, FileDescriptor> fds;
  std::set<File*> mmaps;

  Process(pid_t thread_id, std::string cwd, Command* command) :
      thread_id(thread_id),
      _command(command),
      _cwd(cwd) {}

  Command* getCommand() { return _command; }

  void chdir(std::string newdir) { _cwd = newdir; }

  void chroot(std::string newroot) { _root = newroot; }

  Process* fork(pid_t child_pid) {
    Process* child_proc = new Process(child_pid, _cwd, _command);
    child_proc->fds = fds;
    return child_proc;
  }

  void exec(Trace& trace, std::string exe_path);

 private:
  Command* _command;
  std::string _cwd;
  std::string _root;
};

struct file_comparator {
  bool operator()(File* const& lhs, File* const& rhs) const {
    if(lhs->isPipe()) return true;
    else if(rhs->isPipe()) return false;
    else return lhs->getPath() <= rhs->getPath();
  }
};

struct Trace {
  std::set<File*, file_comparator> files;
  std::vector<File*> latest_versions;
  std::list<Command*> commands;
  ::capnp::MallocMessageBuilder temp_message;

  Trace(std::string starting_dir) : _starting_dir(starting_dir) {}

  Blob getStartingDir() { return stringToBlob(_starting_dir); }

  void newProcess(pid_t pid, Command* cmd) {
    Process* proc = new Process(pid, _starting_dir, cmd);
    _processes.emplace(pid, proc);
    // processes.insert(std::pair<pid_t, Process*>(pid, proc));
  }

  size_t find_file(BlobPtr path) {
    for (size_t index = 0; index < this->latest_versions.size(); index++) {
      if (!this->latest_versions[index]->isPipe() &&
          this->latest_versions[index]->getPath() == blobToString(path)) {
        return index;
      }
    }
    size_t location = this->latest_versions.size();
    File* new_node = new File(location, false, kj::heapArray(path), nullptr, this, nullptr);
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
    File* f = this->latest_versions[file_location];

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
          if (f->known_removed || f->isPipe()) {
            file_exists = false;
          } else {
            struct stat stat_info;
            file_exists = (lstat(f->getPath().c_str(), &stat_info) == 0);
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
    File* f = this->latest_versions[file_location];
    if (is_rewrite && (f->creator != proc->getCommand() || f->writer != nullptr)) {
      // fprintf(stdout, "REWRITE ");
      f = f->make_version();
      f->creator = proc->getCommand();
      f->writer = nullptr;
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

  void add_pipe(pid_t pid, int fds[2], bool cloexec) {
    Process* proc = _processes[pid];
    // fprintf(stdout, "[%d] Pipe %d, %d\n", proc->thread_id, fds[0], fds[1]);
    size_t location = this->latest_versions.size();
    File* p = new File(location, true, Blob(), proc->getCommand(), this, NULL);
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
    File* f = this->latest_versions[desc.location_index];
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

  void add_exec(pid_t pid, std::string exe_path) {
    Process* proc = _processes.at(pid);
    proc->exec(*this, exe_path);
  }

  void add_exec_argument(pid_t pid, Blob&& argument, int index) {
    _processes[pid]->getCommand()->addArgument(blobToString(argument));
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
