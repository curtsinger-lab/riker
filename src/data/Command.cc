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

/// The command accesses an artifact by path.
shared_ptr<Reference> Command::access(string path, AccessFlags flags) {
  auto ref = make_shared<Access>(path, flags);
  _steps.push_back(ref);

  // Handling for the create and truncate flags is in the tracing layer. Currently, this is
  // implemented by tagging a new version of the file immediately after the access.

  return ref;
}

/// This command creates a reference to a new pipe
shared_ptr<Reference> Command::pipe() {
  auto ref = make_shared<Pipe>();
  _steps.push_back(ref);
  return ref;
}

/// This command observes a reference resolve with a particular result
void Command::referenceResult(shared_ptr<Reference> ref, int result) {
  _steps.push_back(make_shared<ReferenceResult>(ref, result));
}

/// This command accesses the metadata for an artifact
void Command::metadataMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Get the version we depend on
  auto v = a->getLatestVersion();

  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && v->getCreator() == shared_from_this()) return;

  // Add this check to the set of metadata checks. If the check is not new, we can return.
  if (options::skip_repeat_checks) {
    auto [_, inserted] = _metadata_checks.emplace(ref, v);
    if (!inserted) return;
  }

  // The version has been accessed
  v->setAccessed();

  // Make sure we have metadata saved for that version
  v->saveMetadata(ref);

  // Record the dependency on metadata
  _steps.push_back(make_shared<MetadataMatch>(ref, a->getLatestVersion()));
}

/// This command accesses the contents of an artifact
void Command::contentsMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // We depend on the latest version of the artifact. Get it now.
  auto v = a->getLatestVersion();

  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && v->getCreator() == shared_from_this()) return;

  // Add this check to the set of contents checks. If the check is not new, we can return.
  if (options::skip_repeat_checks) {
    auto [_, inserted] = _contents_checks.emplace(ref, v);
    if (!inserted) return;
  }

  // The version has been accessed
  v->setAccessed();

  // Make sure we have a fingerprint saved for this version
  v->saveFingerprint(ref);

  // Record the dependency
  _steps.push_back(make_shared<ContentsMatch>(ref, v));
}

/// This command sets the metadata for an artifact
void Command::setMetadata(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // We cannot do write-combining on metadata updates because any access to a path could depend on
  // an update to the metadata of any artifact along that path (e.g. /, /foo, /foo/bar, ...)

  // Tag a new version
  auto new_version = a->tagNewVersion(shared_from_this());

  // Record the update
  _steps.push_back(make_shared<SetContents>(ref, new_version));
}

/// This command sets the contents of an artifact
void Command::setContents(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Get the latest version of this artifact
  auto v = a->getLatestVersion();

  // If this command created the last version, and no other command has accessed it, we can
  // combine the updates into a single update. That means we don't need to tag a new version.
  if (options::combine_writes && v->getCreator() == shared_from_this() && !v->isAccessed()) return;

  // If we reach this point, the command is creating a new version of the artifact
  auto new_version = a->tagNewVersion(shared_from_this());
  _steps.push_back(make_shared<SetContents>(ref, new_version));
}

/// This command launches a child command
shared_ptr<Command> Command::launch(string exe, vector<string> args, map<int, InitialFD> fds) {
  shared_ptr<Command> child(new Command(exe, args, fds));

  if (options::print_on_run) cout << child->getFullName() << endl;

  _steps.push_back(make_shared<Launch>(child));
  _children.push_back(child);
  return child;
}
