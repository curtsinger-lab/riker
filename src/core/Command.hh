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
          unsigned int depth);

 public:
  Command(std::string cmd, const std::list<std::string>& args);

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) = default;
  Command& operator=(Command&&) = default;

  /****** Non-trivial methods ******/

  std::shared_ptr<Command> createChild(std::string cmd, const std::list<std::string>& args);

  void addInput(std::shared_ptr<File> f);

  void addOutput(std::shared_ptr<File> f);

  size_t numDescendants();

  bool canDependOn(const std::shared_ptr<File> f);

  void traceRead(std::shared_ptr<File> f);
  
  void traceModify(std::shared_ptr<File> f);
  
  void traceCreate(std::shared_ptr<File> f);

  void serialize(const Serializer& serializer, db::Command::Builder builder);

  /****** Getters and setters ******/

  const std::string& getExecutable() { return _cmd; }

  const std::list<std::string>& getArguments() { return _args; }

  const std::list<std::shared_ptr<Command>>& getChildren() { return _children; }

  const std::set<std::shared_ptr<File>>& getDeletedFiles() const { return _deleted_files; }
  void addDeletedFile(std::shared_ptr<File> f) { _deleted_files.insert(f); }

  bool getCollapseWithParent() const { return _collapse_with_parent; }

  const std::map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }
  void setInitialFDs(const std::map<int, FileDescriptor>& fds) { _initial_fds = fds; }

  /****** Private methods ******/
 private:
  static void collapse(std::set<std::shared_ptr<Command>>& commands);

  std::shared_ptr<Command> collapseHelper(unsigned int min_depth);

 private:
  std::string _cmd;
  std::list<std::string> _args;
  std::shared_ptr<Command> _parent;
  unsigned int _depth;
  std::list<std::shared_ptr<Command>> _children;
  std::set<std::shared_ptr<File>> _inputs;
  std::set<std::shared_ptr<File>> _outputs;
  std::set<std::shared_ptr<File>> _wr_interactions;
  std::set<std::shared_ptr<File>> _rd_interactions;
  std::set<std::shared_ptr<File>> _deleted_files;
  bool _collapse_with_parent;
  std::map<int, FileDescriptor> _initial_fds;
};
