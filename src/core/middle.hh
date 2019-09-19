#pragma once

#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <capnp/message.h>
#include <capnp/orphan.h>
#include <kj/array.h>

#include "core/command.hh"
#include "core/file.hh"
#include "db/db.capnp.h"

typedef kj::Array<kj::byte> Blob;
typedef kj::ArrayPtr<const kj::byte> BlobPtr;

enum dependency_type { DEP_READ, DEP_MODIFY, DEP_CREATE, DEP_REMOVE };

struct file_reference {
  // fd may be AT_FDCWD and path may be NULL, but not both. If both are present,
  // then path is relative with respect to the directory in fd.
  int fd;
  std::string path;
  // Whether to follow the link if the reference points at a symlink
  bool follow_links;
};

struct File;
struct FileDescriptor;
struct Process;
struct Trace;

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

struct Trace {
  // Because files hold a reference into the memory managed by this message builder, it must appear
  // before the files list to control destructor order. This is a gross hack, and should be fixed.
  ::capnp::MallocMessageBuilder temp_message;

  std::list<File> files;
  std::vector<File*> latest_versions;
  std::list<Command*> commands;

  Trace(std::string starting_dir) : _starting_dir(starting_dir) {}

  std::string getStartingDir() { return _starting_dir; }

  void newProcess(pid_t pid, Command* cmd);

  size_t find_file(std::string path);

  void serialize_graph();

  void add_dependency(pid_t pid, struct file_reference& file, enum dependency_type type);

  void add_chdir(pid_t pid, struct file_reference& file) { _processes[pid]->chdir(file.path); }

  void add_chroot(pid_t pid, struct file_reference& file) { _processes[pid]->chroot(file.path); }

  void add_open(pid_t pid, int fd, struct file_reference& file, int access_mode, bool is_rewrite,
                bool cloexec, mode_t mode);

  void add_pipe(pid_t pid, int fds[2], bool cloexec);

  void add_dup(pid_t pid, int duped_fd, int new_fd, bool cloexec);

  void add_set_cloexec(pid_t pid, int fd, bool cloexec);

  void add_mmap(pid_t pid, int fd);

  void add_close(pid_t pid, int fd) { _processes[pid]->fds.erase(fd); }

  void add_fork(pid_t pid, pid_t child_pid);

  void add_clone(pid_t pid, pid_t thread_id) { _processes[thread_id] = _processes[pid]; }

  void add_exec(pid_t pid, std::string exe_path);

  void add_exec_argument(pid_t pid, std::string argument, int index);

  void add_exit(pid_t pid);

 private:
  std::string _starting_dir;
  std::map<pid_t, Process*> _processes;
};
