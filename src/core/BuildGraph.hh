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
class Graphviz;
class Serializer;
class Tracer;

using std::list;
using std::map;
using std::string;
using std::unique_ptr;
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
  
  void prune();

  void serialize(Serializer& serializer);
  
  void drawGraph(Graphviz& g);

  /****** Getters and setters ******/

  File* getFile(string path, File::Type type = File::Type::UNKNOWN);

  File* getPipe(string name = "pipe");

 private:
  unique_ptr<Command> _root;
  list<File> _files;
  map<string, File*> _current_files;
};
