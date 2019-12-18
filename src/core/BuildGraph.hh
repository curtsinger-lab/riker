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

  /****** Getters and setters ******/

  shared_ptr<Artifact> getArtifact(string path, Artifact::Type type = Artifact::Type::UNKNOWN);

  void linkArtifact(string path, shared_ptr<Artifact> f) { _current_files[path] = f; }
  void unlinkArtifact(string path) { _current_files.erase(path); }

  shared_ptr<Artifact> getPipe(string name = "");

 private:
  shared_ptr<Command> _root;
  map<string, shared_ptr<Artifact>> _current_files;
};
