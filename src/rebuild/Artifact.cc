#include "Artifact.hh"

#include <memory>

#include "data/Command.hh"
#include "data/IR.hh"
#include "data/Version.hh"
#include "ui/options.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::nullopt;
using std::shared_ptr;

// Create a version of this artifact to reflect what is currently on the filesystem
void Artifact::createExistingVersion() {
  // Just create an empty version with no creator
  tagNewVersion(nullptr);
}

// Get the path to this artifact if it has one
optional<string> Artifact::getPath() const {
  if (auto a = dynamic_pointer_cast<Access>(_ref)) {
    return a->getPath();
  } else {
    return nullopt;
  }
}

// Command c accesses this artifact's metadata
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<Version> Artifact::accessMetadata(shared_ptr<Command> c) {
  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && _creator == c) return nullptr;

  // Add this check to the set of metadata checks. If the check is not new, we can return.
  if (options::skip_repeat_checks && metadataAccessedBy(c)) return nullptr;

  // Get the latest version, mark it as accessed, and return it
  _accessed = true;
  return getMetadata();
}

// Command c accesses this artifact's contents
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<Version> Artifact::accessContents(shared_ptr<Command> c) {
  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && _creator == c) return nullptr;

  // Add this check to the set of contents checks. If the check is not new, we can return.
  if (options::skip_repeat_checks && contentsAccessedBy(c)) return nullptr;

  // Get the latest version, mark it as accessed, and return it
  _accessed = true;
  return getContents();
}

// Command c sets the metadata for this artifact.
// Return the version created by this operation, or nullptr if no new version is necessary.
shared_ptr<Version> Artifact::setMetadata(shared_ptr<Command> c) {
  // We cannot do write-combining on metadata updates because any access to a path could depend on
  // an update to the metadata of any artifact along that path (e.g. /, /foo, /foo/bar, ...)

  // Create a new version
  return tagNewVersion(c);
}

// Command c sets the metadata for this artifact to an existing version. Used during emulation.
void Artifact::setMetadata(shared_ptr<Command> c, shared_ptr<Version> v) {
  // The new version has not been accessed
  _metadata_accesses.clear();
  _content_accesses.clear();
  _accessed = false;

  // Update the creator
  _creator = c;

  // Add the verison
  _versions.push_back(v);

  // Identify the version for pretty-printing
  v->identify(this);
}

// Command c sets the contents of this artifact.
// Return the version created by this operation, or nullptr if no new version is necessary.
shared_ptr<Version> Artifact::setContents(shared_ptr<Command> c) {
  // If this command created the last version, and no other command has accessed it, we can
  // combine the updates into a single update. That means we don't need to tag a new version.
  if (options::combine_writes && _creator == c && !_accessed) {
    return nullptr;
  }

  // If we reach this point, the command is creating a new version of the artifact
  return tagNewVersion(c);
}

// Command c sets the contents of this artifact to an existing version. Used during emulation.
void Artifact::setContents(shared_ptr<Command> c, shared_ptr<Version> v) {
  // The new version has not been accessed
  _metadata_accesses.clear();
  _content_accesses.clear();
  _accessed = false;

  // Update the creator
  _creator = c;

  // Add the verison
  _versions.push_back(v);

  // Identify the version for pretty-printing
  v->identify(this);
}

// Tag a new version of this artifact, created by command c
shared_ptr<Version> Artifact::tagNewVersion(shared_ptr<Command> c) {
  // The new version has not been accessed
  _metadata_accesses.clear();
  _content_accesses.clear();
  _accessed = false;

  // Record the creator of the new version
  _creator = c;

  // Create the new version
  auto v = make_shared<Version>();
  _versions.push_back(v);
  v->identify(this);
  return v;
}

// Save metadata for the latest version of this artifact
void Artifact::saveMetadata(shared_ptr<Reference> ref) {
  _versions.back()->saveMetadata(ref);
}

// Save a fingerprint of the contents of the latest version of this artifact
void Artifact::saveFingerprint(shared_ptr<Reference> ref) {
  _versions.back()->saveFingerprint(ref);
}

// Record command c in the set of commands that have accessed this artifact's current metadata.
// If this is the first such access, return true
bool Artifact::metadataAccessedBy(shared_ptr<Command> c) {
  auto [_, inserted] = _metadata_accesses.insert(c);
  return !inserted;
}

// Record command c in the set of commands that have accessed this artifact's current contents.
// If this is the first such access, return tru
bool Artifact::contentsAccessedBy(shared_ptr<Command> c) {
  auto [_, inserted] = _content_accesses.insert(c);
  return !inserted;
}
