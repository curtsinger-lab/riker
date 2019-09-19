#pragma once

#include <cstddef>
#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "core/FileDescriptor.hh"

struct BuildGraph;
struct File;

struct Command {
  Command(BuildGraph& state, std::string cmd, Command* parent, unsigned int depth);

  Command* createChild(std::string cmd);

  void addInput(File* f);

  void addOutput(File* f, size_t file_location);

  size_t descendants(void);

  void collapse(std::set<Command*>* commands);

  Command* collapse_helper(unsigned int min_depth);

  const std::string& getCommand() { return _cmd; }

  const std::vector<std::string>& getArguments() { return _args; }

  void addArgument(std::string arg) { _args.push_back(arg); }

  bool canDependOn(const File* f);
  
  /****** Getters and setters ******/
  
  const std::list<Command*>& getChildren() { return _children; }
  
  const std::set<File*>& getDeletedFiles() const { return _deleted_files; }
  void addDeletedFile(File* f) { _deleted_files.insert(f); }
  
  bool getCollapseWithParent() const { return _collapse_with_parent; }

 private:
  BuildGraph& _state;
  std::string _cmd;
  std::vector<std::string> _args;
  Command* _parent;
  const unsigned int _depth;
  std::list<Command*> _children;
  std::set<File*> _inputs;
  std::set<File*> _outputs;
  std::set<File*> _wr_interactions;
  std::set<File*> _rd_interactions;
  std::set<File*> _deleted_files;
  bool _collapse_with_parent;

 public:
  std::map<int, FileDescriptor> initial_fds;
};
