#pragma once

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <capnp/list.h>
#include <capnp/message.h>
#include <capnp/serialize.h>

#include "core/Command.hh"
#include "core/File.hh"
#include "ui/log.hh"

using std::map;
using std::shared_ptr;
using std::string;

class Serializer {
 public:
  Serializer(string output_path) : _output_path(output_path) {}

  bool hasFile(shared_ptr<File> f) const { return _file_indices.find(f) != _file_indices.end(); }

  size_t getFileIndex(shared_ptr<File> f) const {
    auto iter = _file_indices.find(f);
    FAIL_IF(iter == _file_indices.end())
        << "Tried to serialize reference to untracked file " << f->getPath();
    return iter->second;
  }

  void addFile(shared_ptr<File> f) {
    auto iter = _file_indices.find(f);
    if (iter == _file_indices.end()) {
      size_t index = _file_indices.size();
      _file_indices[f] = index;
    }
  }

  size_t getCommandIndex(shared_ptr<Command> c) const {
    auto iter = _command_indices.find(c);
    FAIL_IF(iter == _command_indices.end())
        << "Tried to serialize reference to untracked command " << c->getExecutable();
    return iter->second;
  }

  void addCommand(shared_ptr<Command> c) {
    auto iter = _command_indices.find(c);
    if (iter == _command_indices.end()) {
      size_t index = _command_indices.size();
      _command_indices[c] = index;
    }

    // Add each child command to the serializer as well
    for (auto child : c->getChildren()) {
      addCommand(child);
    }
  }

  void serialize() {
    capnp::MallocMessageBuilder message;
    db::Graph::Builder graph = message.initRoot<db::Graph>();

    // Reserve space for each file
    auto files = graph.initFiles(_file_indices.size());

    // Serialize files
    for (auto entry : _file_indices) {
      entry.first->serialize(*this, files[entry.second]);
    }

    // Reserve space for each command
    auto commands = graph.initCommands(_command_indices.size());

    // Serialize commands
    for (auto entry : _command_indices) {
      entry.first->serialize(*this, commands[entry.second]);
    }

    // Write the serialized message out to file
    int db_file = open(_output_path.c_str(), O_CREAT | O_TRUNC | O_WRONLY,
                       S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);

    FAIL_IF(db_file < 0) << "Failed to open database file: " << ERR;
    writeMessageToFd(db_file, message);
  }

 private:
  string _output_path;

  map<shared_ptr<File>, size_t> _file_indices;
  map<shared_ptr<Command>, size_t> _command_indices;
};
