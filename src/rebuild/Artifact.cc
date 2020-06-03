#include "Artifact.hh"

#include <memory>

#include "data/Command.hh"
#include "data/IR.hh"
#include "data/Version.hh"
#include "rebuild/Build.hh"
#include "rebuild/Env.hh"
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

  // The latest version has now been accessed
  _accessed = true;

  // Get the version we'll return
  auto result = _versions.back();

  // Inform the environment of this input
  _build->observeMetadataInput(c, shared_from_this());

  // All done
  return result;
}

// Command c checks whether this artifact's metadata matches an expected version
void Artifact::checkMetadata(shared_ptr<Command> c, shared_ptr<Version> v) {
  // Inform the environment of this input
  _build->observeMetadataInput(c, shared_from_this());

  // Compare versions
  if (!_versions.back()->metadataMatch(v)) {
    _build->observeMetadataMismatch(c, shared_from_this());
  }
}

// Command c accesses this artifact's contents
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<Version> Artifact::accessContents(shared_ptr<Command> c) {
  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && _creator == c) return nullptr;

  // Add this check to the set of contents checks. If the check is not new, we can return.
  if (options::skip_repeat_checks && contentsAccessedBy(c)) return nullptr;

  // The latest version has now been accessed
  _accessed = true;

  // Inform the environment of this input
  _build->observeContentInput(c, shared_from_this());

  // All done
  return _versions.back();
}

// Command c checks whether this artifact's content matches an expected version
void Artifact::checkContents(shared_ptr<Command> c, shared_ptr<Version> v) {
  // Inform the environment of this input
  _build->observeContentInput(c, shared_from_this());

  // Compare versions
  if (!_versions.back()->contentsMatch(v)) {
    _build->observeContentMismatch(c, shared_from_this());
  }
}

// Command c sets the metadata for this artifact.
// Return the version created by this operation, or nullptr if no new version is necessary.
shared_ptr<Version> Artifact::setMetadata(shared_ptr<Command> c) {
  // We cannot do write-combining on metadata updates because any access to a path could depend on
  // an update to the metadata of any artifact along that path (e.g. /, /foo, /foo/bar, ...)

  // Create a new version
  auto result = tagNewVersion(c);

  // Inform the environment of this output
  _build->observeMetadataOutput(c, shared_from_this());

  // Return the newly-tagged version
  return result;
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

  // Inform the environment of this output
  _build->observeMetadataOutput(c, shared_from_this());
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
  auto result = tagNewVersion(c);

  // Inform the environment of this output
  _build->observeContentOutput(c, shared_from_this());

  // Return the newly-tagged version
  return result;
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

  // Inform the environment of this output
  _build->observeContentOutput(c, shared_from_this());
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

// Check if this artifact can be restored to the filesystem
bool Artifact::isSaved() const {
  return _versions.back()->isSaved();
}

// Check this artifact's contents and metadata against the filesystem state
void Artifact::checkFinalState(shared_ptr<Reference> ref) {
  // Create a version that represents the on-disk contents reached through this reference
  auto v = make_shared<Version>();
  v->saveMetadata(ref);
  v->saveFingerprint(ref);

  if (!_versions.back()->metadataMatch(v)) {
    _build->observeFinalMetadataMismatch(shared_from_this());
  }

  if (!_versions.back()->contentsMatch(v)) {
    _build->observeFinalContentMismatch(shared_from_this());
  }
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
