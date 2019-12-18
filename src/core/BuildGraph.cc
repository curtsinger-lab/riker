#include "core/BuildGraph.hh"

#include <map>
#include <memory>
#include <string>

#include <fcntl.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::make_shared;
using std::map;
using std::string;

BuildGraph::BuildGraph(string exe) {
  map<int, Artifact::Ref> fds = {{0, Artifact::Ref(getPipe("stdin"), O_RDONLY, false)},
                                 {1, Artifact::Ref(getPipe("stdout"), O_WRONLY, false)},
                                 {2, Artifact::Ref(getPipe("stderr"), O_WRONLY, false)}};
  _root = shared_ptr<Command>(new Command(exe, {exe}, fds));
  INFO << "BuildGraph initialized with root " << _root.get();
}

bool BuildGraph::load(string filename) {
  // No loading yet. Just return failure
  return false;
}

void BuildGraph::run(Tracer& tracer) {
  if (_root) _root->run(tracer);
}

void BuildGraph::prune() {
  if (_root) _root->prune();
}

shared_ptr<Artifact> BuildGraph::getArtifact(string path, Artifact::Type type) {
  auto entry = _current_files.find(path);
  if (entry == _current_files.end()) {
    shared_ptr<Artifact> f = make_shared<Artifact>(path, type);
    _current_files[path] = f;
    return f;
  } else {
    return entry->second;
  }
}

shared_ptr<Artifact> BuildGraph::getPipe(string name) {
  return make_shared<Artifact>(name, Artifact::Type::PIPE);
}

void BuildGraph::drawGraph(Graphviz& g) {
  if (_root) _root->drawGraph(g);
}
