#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "core/FileDescriptor.hh"
#include "db/db.capnp.h"

struct File;
struct Serializer;

struct Command : public std::enable_shared_from_this<Command> {
  /****** Constructors ******/
 private:
  Command(std::string cmd, const std::list<std::string>& args, std::shared_ptr<Command> parent,
          unsigned int depth) :
      _cmd(cmd),
      _args(args),
      _parent(parent),
      _depth(depth) {}

 public:
  Command(std::string cmd, const std::list<std::string>& args) :
      _cmd(cmd),
      _args(args),
      _parent(nullptr),
      _depth(0) {}

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /****** Non-trivial methods ******/

  std::shared_ptr<Command> createChild(std::string cmd, const std::list<std::string>& args);

  size_t numDescendants();

  void traceRead(std::shared_ptr<File> f);

  void traceModify(std::shared_ptr<File> f);

  void traceCreate(std::shared_ptr<File> f);

  void traceRemove(std::shared_ptr<File> f);

  void serialize(const Serializer& serializer, db::Command::Builder builder);

  /****** Getters and setters ******/

  const std::string& getExecutable() const { return _cmd; }

  const std::list<std::string>& getArguments() const { return _args; }

  const std::list<std::shared_ptr<Command>>& getChildren() const { return _children; }

  const std::set<std::shared_ptr<File>>& getDeletedFiles() const { return _deleted_files; }

  const std::map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }

  void setInitialFDs(const std::map<int, FileDescriptor>& fds) { _initial_fds = fds; }

 private:
  std::string _cmd;
  std::list<std::string> _args;
  std::shared_ptr<Command> _parent;
  unsigned int _depth;
  std::list<std::shared_ptr<Command>> _children;
  std::set<std::shared_ptr<File>> _inputs;
  std::set<std::shared_ptr<File>> _outputs;
  std::set<std::shared_ptr<File>> _deleted_files;
  bool _collapse_with_parent;
  std::map<int, FileDescriptor> _initial_fds;
};
