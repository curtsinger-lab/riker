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

  auto stdin_ref = make_shared<Pipe>();
  auto stdout_ref = make_shared<Pipe>();
  auto stderr_ref = make_shared<Pipe>();

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

// This command accesses an artifact by path.
shared_ptr<Access> Command::access(string path, AccessFlags flags) {
  auto ref = make_shared<Access>(path, flags);
  _steps.push_back(ref);
  return ref;
}

// This command creates a reference to a new pipe
shared_ptr<Pipe> Command::pipe() {
  auto ref = make_shared<Pipe>();
  _steps.push_back(ref);
  return ref;
}

// This command observes a reference resolve with a particular result
void Command::referenceResult(shared_ptr<Reference> ref, int result) {
  _steps.push_back(make_shared<ReferenceResult>(ref, result));
}

// This command depends on the metadata of a referenced artifact
void Command::metadataMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Inform the artifact that this command accesses its metadata
  auto v = a->accessMetadata(shared_from_this());

  // If v is a valid version, add this check to the trace IR
  if (v) {
    // Save the version's metadata so we can check it on rebuild
    v->saveMetadata(ref);

    // Add the IR step
    _steps.push_back(make_shared<MetadataMatch>(ref, v));
  }
}

// This command depends on the contents of a referenced artifact
void Command::contentsMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Inform the artifact that this command accesses its contents
  auto v = a->accessContents(shared_from_this());

  // if v is a valid version, add a contents check to the trace IR
  if (v) {
    // Save the version's finerprint so we can check it on rebuild
    v->saveFingerprint(ref);

    // Add the IR step
    _steps.push_back(make_shared<ContentsMatch>(ref, v));
  }
}

// This command sets the metadata of a referenced artifact
void Command::setMetadata(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Inform the artifact that this command sets its metadata
  auto v = a->setMetadata(shared_from_this());

  // If we created a new version, record this action in the trace IR
  if (v) {
    // Create the SetMetadata step and add it to the command
    _steps.push_back(make_shared<SetMetadata>(ref, v));
  }
}

// This command sets the contents of a referenced artifact
void Command::setContents(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Inform the artifact that this command sets its contents
  auto v = a->setContents(shared_from_this());

  // If we created a new version, record this action in the trace IR
  if (v) {
    // Create the SetContents step and add it to the command
    _steps.push_back(make_shared<SetContents>(ref, v));
  }
}

// This command launches a child command
shared_ptr<Command> Command::launch(string exe, vector<string> args, map<int, InitialFD> fds) {
  auto child = make_shared<Command>(exe, args, fds);

  if (options::print_on_run) cout << child->getFullName() << endl;

  _steps.push_back(make_shared<Launch>(child));
  _children.push_back(child);

  return child;
}

// Tell this command that it must rerun, and propagate that mark along the command graph
void Command::mark() {
  // If this command is already marked, there's no work to do
  if (_rerun) return;

  // Mark this command
  _rerun = true;

  // Mark this command's children
  for (auto& child : _children) {
    child->mark();
  }

  // Mark any commands that produce output that this command needs
  for (auto& other : _needs_output_from) {
    other->mark();
  }

  // Mark any commands that use this command's output
  for (auto& other : _output_used_by) {
    other->mark();
  }
}
