#pragma once

#include <list>
#include <map>
#include <memory>
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

#include "core/Command.hh"
#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "core/Process.hh"
#include "db/db.capnp.h"

enum dependency_type { DEP_READ, DEP_MODIFY, DEP_CREATE, DEP_REMOVE };

struct file_reference {
  // fd may be AT_FDCWD and path may be NULL, but not both. If both are present,
  // then path is relative with respect to the directory in fd.
  int fd;
  std::string path;
  // Whether to follow the link if the reference points at a symlink
  bool follow_links;
};

struct BuildGraph {
  // Because files hold a reference into the memory managed by this message builder, it must appear
  // before the files list to control destructor order. This is a gross hack, and should be fixed.
  ::capnp::MallocMessageBuilder temp_message;

  std::list<File> files;
  std::vector<File*> latest_versions;
  std::list<Command*> commands;

  BuildGraph(std::string starting_dir) : _starting_dir(starting_dir) {}

  std::string getStartingDir() { return _starting_dir; }

  void newProcess(pid_t pid, Command* cmd);

  size_t find_file(std::string path);

  void serialize_graph();

  void add_dependency(pid_t pid, struct file_reference& file, enum dependency_type type);

  void add_chdir(pid_t pid, struct file_reference& file);

  void add_chroot(pid_t pid, struct file_reference& file);

  void add_open(pid_t pid, int fd, struct file_reference& file, int access_mode, bool is_rewrite,
                bool cloexec, mode_t mode);

  void add_pipe(pid_t pid, int fds[2], bool cloexec);

  void add_dup(pid_t pid, int duped_fd, int new_fd, bool cloexec);

  void add_set_cloexec(pid_t pid, int fd, bool cloexec);

  void add_mmap(pid_t pid, int fd);

  void add_close(pid_t pid, int fd);

  void add_fork(pid_t pid, pid_t child_pid);

  void add_clone(pid_t pid, pid_t thread_id);

  void add_exec(pid_t pid, std::string exe_path, std::list<std::string> args);

  void add_exit(pid_t pid);

 private:
  std::string _starting_dir;
  std::map<pid_t, std::shared_ptr<Process>> _processes;
};
