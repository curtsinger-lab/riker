#include "core/BuildGraph.hh"

#include <map>
#include <utility>

#include <fcntl.h>

#include "core/File.hh"
#include "core/FileDescriptor.hh"
#include "db/Serializer.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::make_shared;
using std::make_unique;
using std::map;
using std::string;
using std::unique_ptr;

BuildGraph::BuildGraph(string exe) {
  map<int, FileDescriptor> fds = {{0, FileDescriptor(getPipe("<<stdin>>"), O_RDONLY, false)},
                                  {1, FileDescriptor(getPipe("<<stdout>>"), O_WRONLY, false)},
                                  {2, FileDescriptor(getPipe("<<stderr>>"), O_WRONLY, false)}};
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

File* BuildGraph::getFile(string path, File::Type type) {
  auto entry = _current_files.find(path);
  if (entry == _current_files.end()) {
    _files.push_back(File(path, type));
    File* f = &_files.back();
    _current_files[path] = f;
    return f;
  } else {
    return entry->second;
  }
}

File* BuildGraph::getPipe(string name) {
  _files.push_back(File(name, File::Type::PIPE));
  return &_files.back();
}

void BuildGraph::drawGraph(Graphviz& g) {
  for (auto& f : _files) {
    if (!f.isSystemFile() || options.show_sysfiles) {
      f.drawGraph(g);
    }
  }

  if (_root) _root->drawGraph(g);
}

void BuildGraph::serialize(Serializer& serializer) {
  // Add the root command (and its descendants) to the serializer
  serializer.addCommand(_root.get());

  // Run the serialization
  serializer.serialize();
}
