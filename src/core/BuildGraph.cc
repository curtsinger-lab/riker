#include "core/BuildGraph.hh"

#include <map>

#include <fcntl.h>
#include <stdio.h>

#include "core/File.hh"
#include "db/Serializer.hh"
#include "db/db.capnp.h"
#include "tracing/Tracer.hh"

using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;

BuildGraph::BuildGraph(string exe, list<string> args) {
  map<int, FileDescriptor> fds = {{0, FileDescriptor(getPipe("<<stdin>>"), O_RDONLY, false)},
                                  {1, FileDescriptor(getPipe("<<stdout>>"), O_WRONLY, false)},
                                  {2, FileDescriptor(getPipe("<<stderr>>"), O_WRONLY, false)}};
  _root = make_shared<Command>(exe, args, fds);
}

void BuildGraph::run(Tracer& tracer) {
  tracer.run(_root);
}

shared_ptr<File> BuildGraph::getFile(string path) {
  auto entry = _current_files.find(path);
  if (entry == _current_files.end()) {
    auto f = make_shared<File>(path);
    _current_files[path] = f;
    return f;
  } else {
    return entry->second;
  }
}

shared_ptr<File> BuildGraph::getPipe(string name) {
  return make_shared<File>(name, File::Type::PIPE);
}

void BuildGraph::serialize(Serializer& serializer) {
  // Add the root command (and its descendants) to the serializer
  serializer.addCommand(_root);

  // Run the serialization
  serializer.serialize();
}
