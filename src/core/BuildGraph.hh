#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "core/Command.hh"
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
  BuildGraph() {}
  
  BuildGraph(string exe, list<string> args);

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

  shared_ptr<File> getFile(string path);

  shared_ptr<File> getPipe(string name = "");

 private:
  shared_ptr<Command> _root;
  map<string, shared_ptr<File>> _current_files;
};
