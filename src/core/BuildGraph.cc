#include "BuildGraph.hh"

#include <list>
#include <map>
#include <memory>
#include <string>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::list;
using std::make_shared;
using std::map;
using std::string;

BuildGraph::BuildGraph(string executable, vector<string> arguments) {
  map<int, FileDescriptor> fds = {
      {0, FileDescriptor(make_shared<Reference::Pipe>(), _stdin, false)},
      {1, FileDescriptor(make_shared<Reference::Pipe>(), _stdout, true)},
      {2, FileDescriptor(make_shared<Reference::Pipe>(), _stderr, true)},
  };

  _root = make_shared<Command>(executable, arguments, fds);

  INFO << "BuildGraph initialized with root " << _root.get();
}

bool BuildGraph::load(string filename) {
  // No loading yet. Just return failure
  return false;
}

void BuildGraph::run(Tracer& tracer) {
  if (_root) _root->run(tracer);
}

void BuildGraph::drawGraph(Graphviz& g) {
  if (_root) _root->drawGraph(g);
}

void BuildGraph::printTrace(ostream& o) {
  if (_root) _root->printTrace(o);
}
