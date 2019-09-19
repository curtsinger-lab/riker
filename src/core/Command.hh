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

  void add_input(File* f);

  void add_output(File* f, size_t file_location);

  size_t descendants(void);

  void collapse(std::set<Command*>* commands);

  Command* collapse_helper(unsigned int min_depth);

  const std::string& getCommand() { return _cmd; }

  const std::vector<std::string>& getArguments() { return _args; }

  void addArgument(std::string arg) { _args.push_back(arg); }

  bool canDependOn(const File* f);
  
  /****** Getters and setters ******/
  
  const std::list<Command*>& getChildren() { return _children; }

 private:
  BuildGraph& _state;
  std::string _cmd;
  std::vector<std::string> _args;
  Command* _parent;
  const unsigned int _depth;
  std::list<Command*> _children;

 public:
  std::set<File*> inputs;
  std::set<File*> outputs;
  std::set<File*> wr_interactions;
  std::set<File*> rd_interactions;
  std::set<File*> deleted_files;
  bool collapse_with_parent;
  std::map<int, FileDescriptor> initial_fds;
};
