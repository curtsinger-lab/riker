#pragma once

#include <cstddef>
#include <list>
#include <memory>
#include <string>
#include <vector>

#include <sys/types.h>

struct Command;
struct File;
struct Serializer;

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

  void serialize(Serializer& serializer);

  /****** Getters and setters ******/

  void setRootCommand(std::shared_ptr<Command> cmd) { _root_command = cmd; }

  std::string getStartingDir() { return _starting_dir; }

  std::shared_ptr<File> getLatestVersion(size_t index) const { return _latest_versions[index]; }
  void setLatestVersion(size_t index, std::shared_ptr<File> f) { _latest_versions[index] = f; }

  void addFile(std::shared_ptr<File> f) { _files.emplace_front(f); }
  
  std::shared_ptr<File> getFile(std::string path) { return _latest_versions[findFile(path)]; }
  
  std::shared_ptr<File> getPipe(std::shared_ptr<Command> creator);
  
  std::shared_ptr<File> getStdin() const { return _stdin; }
  std::shared_ptr<File> getStdout() const { return _stdout; }
  std::shared_ptr<File> getStderr() const { return _stderr; }

 private:
  std::string _starting_dir;
  std::shared_ptr<Command> _root_command;
  std::vector<std::shared_ptr<File>> _latest_versions;
  std::list<std::shared_ptr<File>> _files;

  std::shared_ptr<File> _stdin;
  std::shared_ptr<File> _stdout;
  std::shared_ptr<File> _stderr;
};
