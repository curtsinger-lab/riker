#pragma once

#include <map>
#include <memory>
#include <string>

#include "core/Artifact.hh"
#include "ui/options.hh"

class Command;
class Graphviz;
class Tracer;

using std::map;
using std::string;
using std::unique_ptr;

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

  void drawGraph(Graphviz& g);

 private:
  shared_ptr<Command> _root;
};
