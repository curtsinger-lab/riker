#pragma once

#include <list>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "core/Command.hh"
#include "core/File.hh"

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

  BuildGraph(string root_command);

  // Disallow Copy
  BuildGraph(const BuildGraph&) = delete;
  BuildGraph& operator=(const BuildGraph&) = delete;

  // Allow Move
  BuildGraph(BuildGraph&&) = default;
  BuildGraph& operator=(BuildGraph&&) = default;

  /****** Non-trivial methods ******/

  bool load(string filename);

  void run(Tracer& tracer);

  void prune();

  void serialize(Serializer& serializer);

  void drawGraph(Graphviz& g);

  /****** Getters and setters ******/

  File* getFile(string path, File::Type type = File::Type::UNKNOWN);

  File* getPipe(string name="");

 private:
  unique_ptr<Command> _root;
  list<File> _files;
  map<string, File*> _current_files;
};
