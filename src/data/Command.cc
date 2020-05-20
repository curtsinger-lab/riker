#include "Command.hh"

#include <array>
#include <cstdlib>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>

#include <limits.h>
#include <unistd.h>

#include "data/AccessFlags.hh"
#include "data/IR.hh"
#include "data/InitialFD.hh"
#include "data/Version.hh"
#include "rebuild/Artifact.hh"
#include "ui/options.hh"

using std::array;
using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::list;
using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;

string readlink(string path) {
  char* buffer = nullptr;
  ssize_t capacity = 0;
  ssize_t bytes_read = 0;

  do {
    capacity += PATH_MAX;
    buffer = (char*)realloc(buffer, capacity);
    bytes_read = readlink(path.c_str(), buffer, capacity);
  } while (bytes_read == capacity);

  string result(buffer, buffer + bytes_read);
  free(buffer);
  return result;
}

// The root command invokes "dodo launch" to run the actual build script. Construct this command.
shared_ptr<Command> Command::createRootCommand() {
  // We need to get the path to dodo. Use readlink for this.
  string dodo = readlink("/proc/self/exe");

  auto stdin_ref = make_shared<Pipe>(nullptr);
  auto stdout_ref = make_shared<Pipe>(nullptr);
  auto stderr_ref = make_shared<Pipe>(nullptr);

  map<int, InitialFD> default_fds = {{0, InitialFD(stdin_ref, false)},
                                     {1, InitialFD(stdout_ref, true)},
                                     {2, InitialFD(stderr_ref, true)}};

  shared_ptr<Command> root(new Command(dodo, {"dodo", "launch"}, default_fds));

  return root;
}

string Command::getShortName() const {
  // By default, the short name is the executable
  auto result = _exe;

  // If we have arguments, use args[0] instead of the exe name
  if (_args.size() > 0) result = _args.front();

  // Strip path from the base name
  auto pos = result.rfind('/');
  if (pos != string::npos) {
    result = result.substr(pos + 1);
  }

  // Add a couple arguments if we have them
  if (_args.size() >= 2) result += " " + _args[1];
  if (_args.size() >= 3) result += " " + _args[2];
  if (_args.size() >= 4) result += " ...";

  return result;
}

string Command::getFullName() const {
  string result;
  bool first = true;
  for (const string& arg : _args) {
    if (!first) result += " ";
    first = false;
    result += arg;
  }
  return result;
}
