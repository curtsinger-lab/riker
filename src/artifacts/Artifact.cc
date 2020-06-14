#include "Artifact.hh"

#include <memory>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "build/Build.hh"
#include "build/Env.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::nullopt;
using std::shared_ptr;

Artifact::Artifact(Env& env, bool committed, shared_ptr<MetadataVersion> v) noexcept : _env(env) {
  appendVersion(v);
  _metadata_version = v;
  _metadata_committed = committed;
}

// Check if an access is allowed by the metadata for this artifact
bool Artifact::checkAccess(AccessFlags flags) noexcept {
  return _metadata_version->checkAccess(flags);
}

// Check if this artifact can be restored to the filesystem
bool Artifact::isSaved() const noexcept {
  // Only the latest metadata version matters
  return _metadata_version->isSaved();
}

// Save the latest metadata version
void Artifact::save(shared_ptr<Reference> ref) noexcept {
  _metadata_version->save(ref);
}

// Check if the latest metadata version is committed
bool Artifact::isCommitted() const noexcept {
  return _metadata_committed;
}

// Commit the latest metadata version
void Artifact::commit(shared_ptr<Reference> ref) noexcept {
  if (!_metadata_committed) {
    ASSERT(_metadata_version->isSaved()) << "Attempted to commit unsaved version";

    _metadata_version->commit(ref);
    _metadata_committed = true;
  }
}

// Check if we have a fingerprint for the latest metadata version
bool Artifact::hasFingerprint() const noexcept {
  return _metadata_version->hasFingerprint();
}

// Save a fingerprint for the latest metadata version
void Artifact::fingerprint(shared_ptr<Reference> ref) noexcept {
  _metadata_version->fingerprint(ref);
}

// Check the final state of this artifact, and save its fingerprint if necessary
void Artifact::finalize(shared_ptr<Reference> ref) noexcept {
  // Is the metadata for this artifact committed?
  if (_metadata_committed) {
    // Yes. We know the on-disk state already matches the latest version.
    // Make sure we have a fingerprint for the metadata version
    _metadata_version->fingerprint(ref);

  } else {
    // No. Check the on-disk version against the expected version
    auto v = make_shared<MetadataVersion>();
    v->fingerprint(ref);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_metadata_version->matches(v)) {
      // Yes. Report the mismatch
      _env.getBuild().observeFinalMismatch(shared_from_this(), _metadata_version, v);
    } else {
      // No. We can treat this artifact as if we committed it
      _metadata_committed = true;
    }
  }
}

// Command c accesses this artifact's metadata
// Return the version it observes, or nullptr if no check is necessary
const shared_ptr<MetadataVersion>& Artifact::accessMetadata(shared_ptr<Command> c,
                                                            shared_ptr<Reference> ref) noexcept {
  // Mark the metadata as accessed
  _metadata_version->accessed();

  // Yes. Notify the build and return the version
  _env.getBuild().observeInput(c, shared_from_this(), _metadata_version);
  return _metadata_version;
}

// Command c sets the metadata for this artifact.
// Return the version created by this operation, or nullptr if no new version is necessary.
const shared_ptr<MetadataVersion>& Artifact::setMetadata(shared_ptr<Command> c,
                                                         shared_ptr<Reference> ref,
                                                         shared_ptr<MetadataVersion> v) noexcept {
  // If no version was provided, the new version will represent what is currently on disk
  if (!v) {
    // Create a version to track the new on-disk state
    _metadata_version = make_shared<MetadataVersion>();

    // Append the new version. This version is committed.
    appendVersion(_metadata_version);
    _metadata_committed = true;

  } else {
    // Adopt v as the new metadata version
    _metadata_version = v;

    // Append the new version. It is NOT committed
    appendVersion(_metadata_version);
    _metadata_committed = false;
  }

  // Record the required information about this metadata update
  _metadata_version->createdBy(c);

  // Inform the environment of this output
  _env.getBuild().observeOutput(c, shared_from_this(), _metadata_version);

  // Return the new metadata version
  return _metadata_version;
}

void Artifact::appendVersion(shared_ptr<Version> v) noexcept {
  _versions.push_back(v);
  v->identify(this);
}
