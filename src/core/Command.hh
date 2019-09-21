#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "core/FileDescriptor.hh"

struct File;

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

  void addInput(File* f);

  void addOutput(File* f);

  size_t descendants();

  void collapse(std::set<std::shared_ptr<Command>>& commands);

  std::shared_ptr<Command> collapse_helper(unsigned int min_depth);

  bool canDependOn(const File* f);

  /****** Getters and setters ******/

  const std::string& getCommand() { return _cmd; }

  const std::list<std::string>& getArguments() { return _args; }

  const std::list<std::shared_ptr<Command>>& getChildren() { return _children; }

  const std::set<File*>& getDeletedFiles() const { return _deleted_files; }
  void addDeletedFile(File* f) { _deleted_files.insert(f); }

  bool getCollapseWithParent() const { return _collapse_with_parent; }

  const std::map<int, FileDescriptor>& getInitialFDs() const { return _initial_fds; }
  void setInitialFDs(const std::map<int, FileDescriptor>& fds) { _initial_fds = fds; }

 private:
  std::string _cmd;
  std::list<std::string> _args;
  std::shared_ptr<Command> _parent;
  unsigned int _depth;
  std::list<std::shared_ptr<Command>> _children;
  std::set<File*> _inputs;
  std::set<File*> _outputs;
  std::set<File*> _wr_interactions;
  std::set<File*> _rd_interactions;
  std::set<File*> _deleted_files;
  bool _collapse_with_parent;
  std::map<int, FileDescriptor> _initial_fds;
};
