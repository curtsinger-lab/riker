#include "Artifact.hh"

#include <memory>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "build/AccessTypes.hh"
#include "build/Build.hh"
#include "build/Env.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::nullopt;
using std::shared_ptr;

Artifact::Artifact(Env& env, shared_ptr<MetadataVersion> v) noexcept : _env(env) {
  appendVersion(v);
  _metadata_version = v;
}

// Check if an access is allowed by the metadata for this artifact
bool Artifact::checkAccess(shared_ptr<Command> c, AccessFlags flags) noexcept {
  //_env.getBuild().observeInput(c, ref, shared_from_this(), _metadata_version,
  //                             InputType::PathResolution);

  // If the current metadata version is committed, make sure we save it for future checks
  // if (_metadata_version->isCommitted()) _metadata_version->save(ref);
  return _metadata_version->checkAccess(flags);
}

// Can this artifact be fully committed?
bool Artifact::canCommit() const noexcept {
  return _metadata_version->canCommit();
}

// Check if the latest metadata version is committed
bool Artifact::isCommitted() const noexcept {
  return _metadata_version->isCommitted();
}

// Commit all final versions of this artifact to the filesystem
void Artifact::commit(fs::path path) noexcept {
  _metadata_version->commit(path);
}

// Compare all final versions of this artifact to the filesystem state
void Artifact::checkFinalState(fs::path path) noexcept {
  if (!_metadata_version->isCommitted()) {
    auto v = make_shared<MetadataVersion>();
    v->fingerprint(path);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_metadata_version->matches(v)) {
      // Yes. Report the mismatch
      _env.getBuild().observeFinalMismatch(shared_from_this(), _metadata_version, v);
    } else {
      // No. We can treat the metadata version as if it is committed
      _metadata_version->setCommitted();
    }
  }
}

// Commit any pending versions and save fingerprints for this artifact
void Artifact::applyFinalState(fs::path path) noexcept {
  // If we don't have a fingerprint of the metadata, take one
  if (!_metadata_version->hasFingerprint()) {
    ASSERT(_metadata_version->isCommitted()) << "Cannot fingerprint an uncommitted version";
    _metadata_version->fingerprint(path);
  }

  // Make sure metadata for this artifact is committed
  _metadata_version->commit(path);
}

/// Get the current metadata version for this artifact
shared_ptr<MetadataVersion> Artifact::getMetadata(shared_ptr<Command> c,
                                                  shared_ptr<Reference> ref,
                                                  InputType t) noexcept {
  // Notify the build of the input
  _env.getBuild().observeInput(c, ref, shared_from_this(), _metadata_version, t);

  // Return the metadata version
  return _metadata_version;
}

/// Check to see if this artifact's metadata matches a known version
void Artifact::match(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<MetadataVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getMetadata(c, ref, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    _env.getBuild().observeMismatch(c, shared_from_this(), observed, expected);
  }
}

/// Apply a new metadata version to this artifact
void Artifact::apply(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<MetadataVersion> writing) noexcept {
  // Update the metadata version for this artifact
  appendVersion(writing);
  _metadata_version = writing;

  // Report the output to the build
  _env.getBuild().observeOutput(c, shared_from_this(), _metadata_version);
}

void Artifact::appendVersion(shared_ptr<Version> v) noexcept {
  _versions.push_back(v);
}

Resolution Artifact::resolve(shared_ptr<Command> c,
                             fs::path resolved,
                             fs::path remaining,
                             shared_ptr<Access> ref,
                             bool committed) noexcept {
  if (remaining.empty()) {
    // Check to see if the requested access mode is supported
    if (!checkAccess(c, ref->getFlags())) return EACCES;
    if (committed) commit(resolved);
    return shared_from_this();
  }

  return ENOTDIR;
}
