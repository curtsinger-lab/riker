#include "core/BuildGraph.hh"

#include "core/File.hh"
#include "db/db.capnp.h"
#include "db/Serializer.hh"
#include "tracing/Tracer.hh"

BuildGraph::BuildGraph(std::string starting_dir) : _starting_dir(starting_dir) {
  size_t stdin_location = _latest_versions.size();
  auto stdin = std::make_shared<File>(*this, stdin_location, db::FileType::PIPE, "<<stdin>>");
  _files.push_front(stdin);
  _latest_versions.push_back(stdin);

  size_t stdout_location = _latest_versions.size();
  auto stdout = std::make_shared<File>(*this, stdout_location, db::FileType::PIPE, "<<stdout>>");
  _files.push_front(stdout);
  _latest_versions.push_back(stdout);

  size_t stderr_location = _latest_versions.size();
  auto stderr = std::make_shared<File>(*this, stderr_location, db::FileType::PIPE, "<<stderr>>");
  _files.push_front(stderr);
  _latest_versions.push_back(stderr);
  
  _default_fds[0] = FileDescriptor(stdin->getLocation(), stdin, O_RDONLY, false);
  _default_fds[1] = FileDescriptor(stdout->getLocation(), stdout, O_WRONLY, false);
  _default_fds[2] = FileDescriptor(stderr->getLocation(), stderr, O_WRONLY, false);
}

void BuildGraph::run(Tracer& tracer) {
  tracer.run(_root);
}

size_t BuildGraph::findFile(std::string path) {
  for (size_t index = 0; index < _latest_versions.size(); index++) {
    if (!_latest_versions[index]->isPipe() && _latest_versions[index]->getPath() == path) {
      return index;
    }
  }
  size_t location = _latest_versions.size();
  std::shared_ptr<File> new_node =
      std::make_shared<File>(*this, location, db::FileType::REGULAR, path, nullptr);
  addFile(new_node);
  _latest_versions.push_back(new_node);
  return location;
}

std::shared_ptr<File> BuildGraph::getPipe(std::shared_ptr<Command> creator) {
  size_t location = _latest_versions.size();
  std::shared_ptr<File> f =
      std::make_shared<File>(*this, location, db::FileType::PIPE, "", creator);
  addFile(f);
  _latest_versions.push_back(f);
  
  return f;
}

void BuildGraph::serialize(Serializer& serializer) {
  // Prepare files for serialization: we've already fingerprinted the old versions,
  // but we need to fingerprint the latest versions
  for (std::shared_ptr<File> f : _latest_versions) {
    f->fingerprint();
  }

  // Add files to the serializer
  for (std::shared_ptr<File> f : _files) {
    // Files can check whether or not they should be saved
    // Also skip the phony files created for stdin, stdout, and stderr
    if (f->shouldSave() && f->getPath() != "<<stdin>>" && f->getPath() != "<<stdout>>" &&
        f->getPath() != "<<stderr>>") {
      serializer.addFile(f);
    }
  }

  // Add the root command (and its descendants) to the serializer
  serializer.addCommand(_root);

  // Run the serialization
  serializer.serialize();
}
