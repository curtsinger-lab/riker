#include "core/BuildGraph.hh"

#include <map>
#include <utility>

#include <fcntl.h>

#include "core/Artifact.hh"
#include "core/FileDescriptor.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"

using std::make_shared;
using std::make_unique;
using std::map;
using std::string;
using std::unique_ptr;

BuildGraph::BuildGraph(string exe) {
  map<int, FileDescriptor> fds = {{0, FileDescriptor(getPipe("stdin"), O_RDONLY, false)},
                                  {1, FileDescriptor(getPipe("stdout"), O_WRONLY, false)},
                                  {2, FileDescriptor(getPipe("stderr"), O_WRONLY, false)}};
  _root = unique_ptr<Command>(new Command(exe, {exe}, fds));
  INFO << "BuildGraph initialized with root " << _root.get();
}

bool BuildGraph::load(string filename) {
  // No loading yet. Just return failure
  return false;
}

void BuildGraph::run(Tracer& tracer) {
  if (_root) tracer.run(_root.get());
}

void BuildGraph::prune() {
  if (_root) _root->prune();
}

Artifact* BuildGraph::getArtifact(string path, Artifact::Type type) {
  auto entry = _current_files.find(path);
  if (entry == _current_files.end()) {
    _files.push_back(Artifact(path, type));
    Artifact* f = &_files.back();
    _current_files[path] = f;
    return f;
  } else {
    return entry->second;
  }
}

Artifact* BuildGraph::getPipe(string name) {
  _files.push_back(Artifact(name, Artifact::Type::PIPE));
  return &_files.back();
}

void BuildGraph::drawGraph(Graphviz& g) {
  if (_root) _root->drawGraph(g);
}
