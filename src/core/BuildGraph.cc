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
  // Create a reference and artifact for each of the standard pipes
  _stdin_ref = make_shared<Reference::Pipe>();
  _stdin = make_shared<Artifact>("<stdin>");

  _stdout_ref = make_shared<Reference::Pipe>();
  _stdout = make_shared<Artifact>("<stdout>");

  _stderr_ref = make_shared<Reference::Pipe>();
  _stderr = make_shared<Artifact>("<stderr>");

  // Build the map of initial file descriptors
  map<int, FileDescriptor> fds = {
      {0, FileDescriptor(_stdin_ref, _stdin, false)},
      {1, FileDescriptor(_stdout_ref, _stdout, true)},
      {2, FileDescriptor(_stderr_ref, _stderr, true)},
  };

  // Create the root command for the build
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
  if (_root) {
    o << _stdin_ref << endl;
    o << _stdout_ref << endl;
    o << _stderr_ref << endl;
    _root->printTrace(o);
  }
}
