#include "Command.hh"

#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "versions/Version.hh"

using std::cout;
using std::endl;
using std::make_shared;
using std::map;
using std::nullopt;
using std::set;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

string Command::getShortName(size_t limit) const noexcept {
  // A command with no arguments is anonymous. This shouldn't happen, but better to be safe.
  if (_args.size() == 0) return "<anon>";

  // The first argument to the command is its name. Treat it as a path for now
  fs::path exe_path = _args.front();
  if (exe_path.is_absolute()) exe_path = exe_path.filename();

  // The output starts with the executable name
  string result = exe_path;

  // Add arguments up to the length limit
  size_t index = 1;
  while (index < _args.size() && result.length() < limit) {
    result += " " + _args[index];
    index++;
  }

  if (limit > 0 && result.length() >= limit) {
    result = result.substr(0, limit - 3) + "...";
  }

  return result;
}

string Command::getFullName() const noexcept {
  string result;
  bool first = true;
  for (const string& arg : _args) {
    if (!first) result += " ";
    first = false;
    result += arg;
  }
  return result;
}

bool Command::isMake() const noexcept {
  fs::path exe_path = _args.front();
  return exe_path.filename().string() == "make";
}

Command::ChildRecord::ChildRecord(shared_ptr<Command> child) noexcept :
    ChildRecord(child->_exe, child->_initial_cwd, child->_args, child->_initial_fds) {
  _command = child;
}

Command::ChildRecord::ChildRecord(shared_ptr<RefResult> exe_ref,
                                  shared_ptr<RefResult> cwd_ref,
                                  vector<string> args,
                                  map<int, FileDescriptor> fds) noexcept {
  auto exe = exe_ref->getArtifact();
  _exe_content = exe ? exe->peekContent() : nullptr;

  auto cwd = cwd_ref->getArtifact();
  _cwd_path = cwd ? cwd->getPath() : nullopt;

  _args = args;
  for (auto& [fd, desc] : fds) {
    auto a = desc.getRef()->getArtifact();
    _fd_content[fd] = a ? a->peekContent() : nullptr;
  }
}

bool Command::ChildRecord::operator==(const ChildRecord& other) noexcept {
  // Does this record have an executable content version?
  if (!_exe_content) return false;

  // Does the other record have an executable content version?
  if (!other._exe_content) return false;

  // Do the executables match?
  if (!_exe_content->matches(other._exe_content)) return false;

  // Do this record have a cwd path?
  if (!_cwd_path.has_value()) return false;

  // Does the other record have a cwd path?
  if (!other._cwd_path.has_value()) return false;

  // Do the cwd paths match?
  if (_cwd_path.value() != other._cwd_path.value()) return false;

  // Do the arguments match?
  if (_args != other._args) return false;

  // Do the records have the same number of file descriptors?
  if (_fd_content.size() != other._fd_content.size()) return false;

  // Do all the file descriptors match?
  for (auto& [fd, v] : _fd_content) {
    // Do we have a version for this record?
    if (!v) return false;

    // Does the other record have the same fd?
    auto iter = other._fd_content.find(fd);
    if (iter == other._fd_content.end()) return false;

    // Does the other record have a version for the corresponding fd?
    if (!iter->second) return false;

    // Do the versions at the two file descriptors match?
    if (!v->matches(iter->second)) return false;
  }

  return true;
}

void Command::addChild(shared_ptr<Command> child) noexcept {
  _children.emplace_back(child);
}

shared_ptr<Command> Command::findChild(shared_ptr<RefResult> exe_ref,
                                       vector<string> args,
                                       map<int, FileDescriptor> fds,
                                       shared_ptr<RefResult> cwd_ref,
                                       shared_ptr<RefResult> root_ref) noexcept {
  ChildRecord record(exe_ref, cwd_ref, args, fds);

  // Loop over the records of children to see if we have a match
  for (auto c : _children) {
    // If the record matches, update the child command to use the new references
    if (c == record) {
      c._command->_exe = exe_ref;
      c._command->_initial_cwd = cwd_ref;
      c._command->_initial_root = root_ref;
      c._command->_initial_fds = fds;

      return c._command;
    }
  }

  return nullptr;
}
