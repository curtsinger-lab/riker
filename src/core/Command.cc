#include "Command.hh"

#include <array>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"
#include "core/Build.hh"
#include "core/IR.hh"
#include "core/Rebuild.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"
#include "util/util.hh"

using std::array;
using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::list;
using std::map;
using std::shared_ptr;
using std::string;

// The root command invokes "dodo launch" to run the actual build script. Construct this command.
shared_ptr<Command> Command::createRootCommand(map<int, FileDescriptor> fds) {
  // We need to get the path to dodo. Use readlink for this.
  string dodo = readlink("/proc/self/exe");

  shared_ptr<Command> root(new Command(dodo, {"dodo", "launch"}, fds));

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

void Command::run(Rebuild& rebuild, Tracer& tracer) {
  if (rebuild.mustRerun(shared_from_this())) {
    // We are rerunning this command, so clear the lists of steps and children
    _steps.clear();
    _children.clear();

    // Show the command if printing is on, or if this is a dry run
    if (Build::print_on_run || Build::dry_run) cout << getFullName() << endl;

    // Actually run the command, unless this is a dry run
    if (!Build::dry_run) tracer.run(shared_from_this());

  } else {
    // Emulate this command by running its children
    for (auto& c : _children) {
      c->run(rebuild, tracer);
    }
  }
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

/// This command requires that a reference resolves to an artifact without failure
void Command::isOK(shared_ptr<Reference> ref) {
  _steps.push_back(make_shared<ReferenceResult>(ref, SUCCESS));
}

/// This command requires that a reference fails to resolve with a specific error
void Command::isError(shared_ptr<Reference> ref, int err) {
  _steps.push_back(make_shared<ReferenceResult>(ref, err));
}

/// This command accesses the metadata for an artifact
void Command::metadataMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // Get the version we depend on
  auto v = a->getLatestVersion();

  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (Build::ignore_self_reads && v.getCreator() == shared_from_this()) return;

  // Add this check to the set of metadata checks. If the check is not new, we can return.
  if (Build::skip_repeat_checks) {
    auto [_, inserted] = _metadata_checks.emplace(ref, v);
    if (!inserted) return;
  }

  // The version has been accessed
  v.setAccessed();

  // Make sure we have metadata saved for that version
  v.saveMetadata();

  // Record the dependency on metadata
  _steps.push_back(make_shared<MetadataMatch>(ref, a->getLatestVersion()));
}

/// This command accesses the contents of an artifact
void Command::contentsMatch(shared_ptr<Reference> ref, shared_ptr<Artifact> a) {
  // We depend on the latest version of the artifact. Get it now.
  auto v = a->getLatestVersion();

  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (Build::ignore_self_reads && v.getCreator() == shared_from_this()) return;

  // Add this check to the set of contents checks. If the check is not new, we can return.
  if (Build::skip_repeat_checks) {
    auto [_, inserted] = _contents_checks.emplace(ref, v);
    if (!inserted) return;
  }

  // The version has been accessed
  v.setAccessed();

  // Make sure we have a fingerprint saved for this version
  v.saveFingerprint();

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
  // Does this artifact have any versions yet?
  if (a->getVersionCount() == 0) {
    // If we're writing to an artifact with no existing versions, our write will create one
    auto new_version = a->tagNewVersion(shared_from_this());
    _steps.push_back(make_shared<SetContents>(ref, new_version));

  } else {
    // If there are already versions, we have to tag a new version unless we can combine this with
    // a previous write by the same command

    // Get the latest version of this artifact
    auto v = a->getLatestVersion();

    // If this command created the last version, and no other command has accessed it, we can
    // combine the updates into a single update. That means we don't need to tag a new version.
    if (Build::combine_writes && v.getCreator() == shared_from_this() && !v.isAccessed()) return;

    // If we reach this point, the command is creating a new version of the artifact
    auto new_version = a->tagNewVersion(shared_from_this());
    _steps.push_back(make_shared<SetContents>(ref, new_version));
  }
}

/// This command launches a child command
shared_ptr<Command> Command::launch(string exe, vector<string> args, map<int, FileDescriptor> fds) {
  shared_ptr<Command> child(new Command(exe, args, fds));

  if (Build::print_on_run) cout << child->getFullName() << endl;

  _steps.push_back(make_shared<Launch>(child));
  _children.push_back(child);
  return child;
}
