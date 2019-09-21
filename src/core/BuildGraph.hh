#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <sys/types.h>

#include "core/File.hh"

struct Command;
struct Process;

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
  /****** Constructors ******/
  
  BuildGraph(std::string starting_dir) : _starting_dir(starting_dir) {}
  
  // Disallow Copy
  BuildGraph(const BuildGraph&) = delete;
  BuildGraph& operator=(const BuildGraph&) = delete;

  // Allow Move
  BuildGraph(BuildGraph&&) = default;
  BuildGraph& operator=(BuildGraph&&) = default;
  
  /****** Non-trivial methods ******/
  
  void newProcess(pid_t pid, Command* cmd);
  
  size_t findFile(std::string path);
  
  void serializeGraph();
  
  /****** Tracing methods ******/
  
  void add_dependency(pid_t pid, struct file_reference& file, enum dependency_type type);

  void traceChdir(pid_t pid, std::string path);

  void traceChroot(pid_t pid, std::string path);

  void add_open(pid_t pid, int fd, struct file_reference& file, int access_mode, bool is_rewrite,
                bool cloexec, mode_t mode);

  void add_pipe(pid_t pid, int fds[2], bool cloexec);

  void add_dup(pid_t pid, int duped_fd, int new_fd, bool cloexec);

  void add_set_cloexec(pid_t pid, int fd, bool cloexec);

  void add_mmap(pid_t pid, int fd);

  void traceClose(pid_t pid, int fd);

  void traceFork(pid_t pid, pid_t child_pid);

  void traceClone(pid_t pid, pid_t thread_id);

  void traceExec(pid_t pid, std::string executable, const std::list<std::string>& args);

  void traceExit(pid_t pid);
  
  /****** Getters and setters ******/

  std::string getStartingDir() { return _starting_dir; }
  
  void addCommand(Command* c) { _commands.push_front(c); }
  
  File* getLatestVersion(size_t index) const { return _latest_versions[index]; }
  void setLatestVersion(size_t index, File* f) { _latest_versions[index] = f; }
  
  File& addFile(File&& f) { return files.emplace_front(std::move(f)); }

 private:
  std::string _starting_dir;
  std::map<pid_t, std::shared_ptr<Process>> _processes;
  std::list<Command*> _commands;
  std::vector<File*> _latest_versions;
  std::list<File> files;
};
