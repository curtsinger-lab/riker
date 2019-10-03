#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "core/FileDescriptor.hh"

class Command;
class File;
class Serializer;
class Tracer;

class BuildGraph {
 public:
  /****** Constructors ******/

  BuildGraph(std::string starting_dir);

  // Disallow Copy
  BuildGraph(const BuildGraph&) = delete;
  BuildGraph& operator=(const BuildGraph&) = delete;

  // Allow Move
  BuildGraph(BuildGraph&&) = default;
  BuildGraph& operator=(BuildGraph&&) = default;

  /****** Non-trivial methods ******/

  void run(Tracer& tracer);

  size_t findFile(std::string path);

  void serialize(Serializer& serializer);

  /****** Getters and setters ******/

  void setRootCommand(std::shared_ptr<Command> cmd) { _root = cmd; }

  std::string getStartingDir() { return _starting_dir; }

  std::shared_ptr<File> getLatestVersion(size_t index) const { return _latest_versions[index]; }
  void setLatestVersion(size_t index, std::shared_ptr<File> f) { _latest_versions[index] = f; }

  void addFile(std::shared_ptr<File> f) { _files.emplace_front(f); }

  std::shared_ptr<File> getFile(std::string path) { return _latest_versions[findFile(path)]; }

  std::shared_ptr<File> getPipe(std::shared_ptr<Command> creator);

  std::map<int, FileDescriptor> getDefaultFds() const { return _default_fds; }

 private:
  std::string _starting_dir;
  std::shared_ptr<Command> _root;
  std::vector<std::shared_ptr<File>> _latest_versions;
  std::list<std::shared_ptr<File>> _files;
  std::map<int, FileDescriptor> _default_fds;
};
