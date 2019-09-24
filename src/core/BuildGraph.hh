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

  BuildGraph(std::string starting_dir);

  // Disallow Copy
  BuildGraph(const BuildGraph&) = delete;
  BuildGraph& operator=(const BuildGraph&) = delete;

  // Allow Move
  BuildGraph(BuildGraph&&) = default;
  BuildGraph& operator=(BuildGraph&&) = default;

  /****** Non-trivial methods ******/

  void newProcess(pid_t pid, std::shared_ptr<Command> cmd);

  size_t findFile(std::string path);

  void serializeGraph();

  /****** Tracing methods ******/

  void traceRead(pid_t pid, struct file_reference& file);

  void traceModify(pid_t pid, struct file_reference& file);

  void traceCreate(pid_t pid, struct file_reference& file);

  void traceRemove(pid_t pid, struct file_reference& file);

  void traceChdir(pid_t pid, std::string path);

  void traceChroot(pid_t pid, std::string path);

  void traceOpen(pid_t pid, int fd, std::string path, int flags, mode_t mode);

  void tracePipe(pid_t pid, int fds[2], bool cloexec);

  void traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec);

  void traceSetCloexec(pid_t pid, int fd, bool cloexec);

  void traceMmap(pid_t pid, int fd);

  void traceClose(pid_t pid, int fd);

  void traceFork(pid_t pid, pid_t child_pid);

  void traceClone(pid_t pid, pid_t thread_id);

  void traceExec(pid_t pid, std::string executable, const std::list<std::string>& args);

  void traceExit(pid_t pid);

  /****** Getters and setters ******/

  std::string getStartingDir() { return _starting_dir; }

  void addCommand(std::shared_ptr<Command> c) { _commands.push_front(c); }

  std::shared_ptr<File> getLatestVersion(size_t index) const { return _latest_versions[index]; }
  void setLatestVersion(size_t index, std::shared_ptr<File> f) { _latest_versions[index] = f; }

  void addFile(std::shared_ptr<File> f) { _files.emplace_front(f); }

 private:
  std::string _starting_dir;
  std::map<pid_t, std::shared_ptr<Process>> _processes;
  std::list<std::shared_ptr<Command>> _commands;
  std::vector<std::shared_ptr<File>> _latest_versions;
  std::list<std::shared_ptr<File>> _files;

  std::shared_ptr<File> _stdin;
  std::shared_ptr<File> _stdout;
  std::shared_ptr<File> _stderr;
};
