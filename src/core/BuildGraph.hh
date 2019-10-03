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

using std::list;
using std::map;
using std::shared_ptr;
using std::string;
using std::vector;

class BuildGraph {
 public:
  /****** Constructors ******/

  BuildGraph(string starting_dir);

  // Disallow Copy
  BuildGraph(const BuildGraph&) = delete;
  BuildGraph& operator=(const BuildGraph&) = delete;

  // Allow Move
  BuildGraph(BuildGraph&&) = default;
  BuildGraph& operator=(BuildGraph&&) = default;

  /****** Non-trivial methods ******/

  void run(Tracer& tracer);

  void serialize(Serializer& serializer);

  /****** Getters and setters ******/

  void setRootCommand(shared_ptr<Command> cmd) { _root = cmd; }

  string getStartingDir() { return _starting_dir; }

  shared_ptr<File> getLatestVersion(size_t index) const { return _latest_versions[index]; }
  void setLatestVersion(size_t index, shared_ptr<File> f) { _latest_versions[index] = f; }

  void addFile(shared_ptr<File> f) { _files.emplace_front(f); }

  shared_ptr<File> getFile(string path);

  shared_ptr<File> getPipe(shared_ptr<Command> creator);

  map<int, FileDescriptor> getDefaultFds() const { return _default_fds; }

 private:
  string _starting_dir;
  shared_ptr<Command> _root;
  vector<shared_ptr<File>> _latest_versions;
  list<shared_ptr<File>> _files;
  map<int, FileDescriptor> _default_fds;
};
