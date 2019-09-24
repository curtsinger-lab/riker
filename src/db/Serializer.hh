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

struct Serializer {
 public:
  Serializer(std::string output_path) : _output_path(output_path) {}

  bool hasFile(std::shared_ptr<File> f) const {
    return _file_indices.find(f) != _file_indices.end();
  }

  size_t getFileIndex(std::shared_ptr<File> f) const {
    auto iter = _file_indices.find(f);
    FAIL_IF(iter == _file_indices.end())
        << "Tried to serialize reference to untracked file " << f->getPath();
    return iter->second;
  }

  void addFile(std::shared_ptr<File> f) {
    auto iter = _file_indices.find(f);
    if (iter == _file_indices.end()) {
      size_t index = _file_indices.size();
      _file_indices[f] = index;

      _input_count += f->getReaders().size();
      if (f->isWritten()) _output_count++;
      if (f->isCreated()) _create_count++;
      if (f->isModified()) _modify_count++;
    }
  }

  size_t getCommandIndex(std::shared_ptr<Command> c) const {
    auto iter = _command_indices.find(c);
    FAIL_IF(iter == _command_indices.end())
        << "Tried to serialize reference to untracked command " << c->getExecutable();
    return iter->second;
  }

  void addCommand(std::shared_ptr<Command> c) {
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

    // Reserve space for dependency edges
    auto inputs = graph.initInputs(_input_count);
    size_t input_index = 0;

    auto outputs = graph.initOutputs(_output_count);
    size_t output_index = 0;

    auto creations = graph.initCreations(_create_count);
    size_t create_index = 0;

    auto modifications = graph.initModifications(_modify_count);
    size_t modify_index = 0;

    // Serialize dependency edges
    for (auto iter : _file_indices) {
      auto file = iter.first;
      auto file_id = iter.second;

      for (auto user : file->getReaders()) {
        inputs[input_index].setInputID(file_id);
        inputs[input_index].setOutputID(getCommandIndex(user));
        input_index++;
      }

      if (file->isWritten()) {
        outputs[output_index].setInputID(getCommandIndex(file->getWriter()));
        outputs[output_index].setOutputID(file_id);
        output_index++;
      }

      if (file->isCreated()) {
        creations[create_index].setInputID(getCommandIndex(file->getCreator()));
        creations[create_index].setOutputID(file_id);
        create_index++;
      }

      if (file->isModified()) {
        modifications[modify_index].setInputID(getFileIndex(file->getPreviousVersion()));
        modifications[modify_index].setOutputID(file_id);
        modify_index++;
      }
    }

    // Serialize removal edges
    size_t removal_count = 0;
    for (auto c : _command_indices) {
      for (auto f : c.first->getDeletedFiles()) {
        if (hasFile(f)) removal_count++;
      }
    }

    auto removals = graph.initRemovals(removal_count);
    size_t removal_index = 0;
    for (auto c : _command_indices) {
      for (auto f : c.first->getDeletedFiles()) {
        if (hasFile(f)) {
          removals[removal_index].setInputID(c.second);
          removals[removal_index].setOutputID(getFileIndex(f));
          removal_index++;
        }
      }
    }

    // Write the serialized message out to file
    int db_file = open(_output_path.c_str(), O_CREAT | O_TRUNC | O_WRONLY,
                       S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);

    FAIL_IF(db_file < 0) << "Failed to open database file: " << ERR;
    writeMessageToFd(db_file, message);
  }

 private:
  std::string _output_path;

  std::map<std::shared_ptr<File>, size_t> _file_indices;
  std::map<std::shared_ptr<Command>, size_t> _command_indices;

  size_t _input_count = 0;
  size_t _output_count = 0;
  size_t _create_count = 0;
  size_t _modify_count = 0;
};
