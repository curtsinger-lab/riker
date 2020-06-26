#include "Command.hh"

#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Build.hh"
#include "core/AccessFlags.hh"
#include "ui/options.hh"
#include "util/path.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::cout;
using std::endl;
using std::make_shared;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

string Command::getShortName(size_t limit) const noexcept {
  // By default, the short name is the executable
  auto exe_path = _exe->getFullPath();

  // If we have arguments, use args[0] instead of the exe name
  if (_args.size() > 0) exe_path = _args.front();

  // If the exe_path is an absolute path, use the filename
  if (exe_path.is_absolute()) exe_path = exe_path.filename();

  // The output starts with the executable name
  string result = exe_path;

  // Add arguments up to the length limit
  size_t index = 1;
  while (index < _args.size() && result.length() < limit) {
    result += " " + _args[index];
    index++;
  }

  if (result.length() >= limit) {
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
