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

Artifact::Artifact(Env& env, bool committed, shared_ptr<MetadataVersion> v) : _env(env) {
  appendVersion(v);
  _metadata_version = v;
  _metadata_committed = committed;
}

// Check if this artifact can be restored to the filesystem
bool Artifact::isSaved() const {
  // Only the latest metadata version matters
  return _metadata_version->isSaved();
}

// Save the latest metadata version
void Artifact::save(const shared_ptr<Reference>& ref) {
  _metadata_version->save(ref);
}

// Check if the latest metadata version is committed
bool Artifact::isCommitted() const {
  return _metadata_committed;
}

// Commit the latest metadata version
void Artifact::commit(const shared_ptr<Reference>& ref) {
  if (!_metadata_committed) {
    FAIL_IF(!_metadata_version->isSaved()) << "Attempted to commit unsaved version";

    _metadata_version->commit(ref);
    _metadata_committed = true;
  }
}

// Check if we have a fingerprint for the latest metadata version
bool Artifact::hasFingerprint() const {
  return _metadata_version->hasFingerprint();
}

// Save a fingerprint for the latest metadata version
void Artifact::fingerprint(const shared_ptr<Reference>& ref) {
  _metadata_version->fingerprint(ref);
}

// Check this artifact's contents and metadata against the filesystem state
void Artifact::checkFinalState(const shared_ptr<Reference>& ref) {
  // We can skip checks if we already know metadata is in a committed state
  if (!_metadata_committed) {
    // Create a version that represents the on-disk contents reached through this reference
    auto v = make_shared<MetadataVersion>();
    v->fingerprint(ref);

    // Report a metadata mismatch if necessary
    if (!_metadata_version->matches(v)) {
      _env.getBuild().observeFinalMismatch(shared_from_this(), _metadata_version, v);
    } else {
      // Since the metadata matches what is on disk, we can treat this artifact as if it was
      // committed
      _metadata_committed = true;
    }
  }
}

// Command c accesses this artifact's metadata
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<MetadataVersion> Artifact::accessMetadata(const shared_ptr<Command>& c,
                                                     const shared_ptr<Reference>& ref) {
  // Mark the metadata as accessed
  _metadata_accessed = true;

  // Yes. Notify the build and return the version
  _env.getBuild().observeInput(c, shared_from_this(), _metadata_version);
  return _metadata_version;
}

// Command c sets the metadata for this artifact.
// Return the version created by this operation, or nullptr if no new version is necessary.
shared_ptr<MetadataVersion> Artifact::setMetadata(const shared_ptr<Command>& c,
                                                  const shared_ptr<Reference>& ref,
                                                  const shared_ptr<MetadataVersion>& v) {
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
  _metadata_creator = c;
  _metadata_ref = ref;
  _metadata_accessed = false;

  // Inform the environment of this output
  _env.getBuild().observeOutput(c, shared_from_this(), _metadata_version);

  // Return the new metadata version
  return _metadata_version;
}

void Artifact::appendVersion(const shared_ptr<Version>& v) {
  _versions.push_back(v);
  v->identify(this);
}
