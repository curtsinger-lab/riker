#include "Command.hh"

#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "core/AccessFlags.hh"
#include "core/Artifact.hh"
#include "core/Build.hh"
#include "core/IR.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"

using std::dynamic_pointer_cast;
using std::list;
using std::map;
using std::shared_ptr;
using std::string;

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

  return result;
}

string Command::getFullName() const {
  string result;
  for (const string& arg : _args) {
    result += arg + " ";
  }
  return result;
}

void Command::run(Tracer& tracer) {
  // TODO: checking logic goes here
  // simulate all of the steps:
  //   references: check that access remains the same
  //   predicates: still hold
  //   action: simulate effect of actions
  //     two things to check:
  //     1. whether the action had the same effect as before
  //     2. if the action had the same effect, what the effect actually is
  //     NOTE: use recursive state environment

  // We are rerunning this command, so clear the list of steps
  _steps.clear();

  // Actually run the command
  tracer.run(shared_from_this());
}

// Check the state of all the inputs to this command and its descendants. Add any commands with
// changed inputs to the result set
void Command::checkInputs(Env& env, set<shared_ptr<Command>>& result) {
  // Notify the environment that we're running this command
  env.startCommand(shared_from_this());

  bool changed = false;
  for (auto s : _steps) {
    if (!s->eval(env)) changed = true;

    // Check child commands as well
    if (auto launch = dynamic_pointer_cast<Launch>(s)) {
      // Record this command's child
      hasChild(launch->getCommand());

      // Run through the child command's trace
      launch->getCommand()->checkInputs(env, result);
    }
  }

  // If anything changed, add this command to the result set
  if (changed) {
    result.insert(shared_from_this());
  }

  // This command is now finished. Notify the environment.
  env.finishCommand();
}

// Mark this command for rerun. If this is a new marking, propagate it to other commands
void Command::mark(set<shared_ptr<Command>>& marked) {
  // If this command was already marked, return
  if (_marked) return;

  // Otherwise, we have work to do. First, mark this command and add it to the marked set
  _marked = true;
  marked.insert(shared_from_this());

  // Rerunning this command also reruns its children
  for (auto& c : _children) {
    c->mark(marked);
  }

  // Rerunning this command requires rerunning any other command whose input we need
  for (auto& c : _needs) {
    c->mark(marked);
  }

  // Rerunning this command requires rerunning any command that uses this command's output
  for (auto& c : _triggers) {
    c->mark(marked);
  }
}

void Command::getMarkedAncestors(set<shared_ptr<Command>>& marked) {
  if (_marked) {
    marked.insert(shared_from_this());
  } else {
    for (auto& c : _children) {
      c->getMarkedAncestors(marked);
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
void Command::launch(shared_ptr<Command> cmd) {
  _steps.push_back(make_shared<Launch>(cmd));
}
