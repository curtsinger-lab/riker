#include "FileArtifact.hh"

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Build.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::shared_ptr;
using std::string;

FileArtifact::FileArtifact(Env& env,
                           shared_ptr<MetadataVersion> mv,
                           shared_ptr<ContentVersion> cv) noexcept :
    Artifact(env, mv) {
  appendVersion(cv);
  _content_version = cv;
}

// Do we have saved content and metadata for this artifact?
bool FileArtifact::isSaved() const noexcept {
  return _content_version->canCommit() && Artifact::isSaved();
}

// Check if the latest version of this artifact are committed to disk
bool FileArtifact::isCommitted() const noexcept {
  return _content_version->isCommitted() && Artifact::isCommitted();
}

// Commit the latest version of this artifact to the filesystem
void FileArtifact::commit(shared_ptr<Reference> ref) noexcept {
  _content_version->commit(ref);

  // Delegate metadata commits to the artifact
  Artifact::commit(ref);
}

void FileArtifact::finalize(shared_ptr<Reference> ref, bool commit) noexcept {
  // Are all content versions committed?
  if (_content_version->isCommitted()) {
    // Yes. Just make sure we have a saved fingerprint
    _content_version->fingerprint(ref);

  } else {
    // No. Check the on-disk version against the modeled version
    auto v = make_shared<ContentVersion>();
    v->fingerprint(ref);

    // Report a content mismatch if necessary
    if (!_content_version->matches(v)) {
      _env.getBuild().observeFinalMismatch(shared_from_this(), _content_version, v);
    } else {
      // Since the contents match what is on disk, we can treat this artifact as if it was committed
      _content_version->setCommitted();
    }
  }

  // If requested, commit all final state to the filesystem
  if (commit) this->commit(ref);

  // Check metadata in the top-level artifact
  Artifact::finalize(ref, commit);
}

void FileArtifact::needsCurrentVersions(shared_ptr<Command> c) noexcept {
  _env.getBuild().observeInput(c, shared_from_this(), _content_version, InputType::Inherited);
  Artifact::needsCurrentVersions(c);
}

/// Get the current content version for this artifact
shared_ptr<ContentVersion> FileArtifact::getContent(shared_ptr<Command> c, InputType t) noexcept {
  // Mark the metadata as accessed
  _content_version->accessed();

  // Notify the build of the input
  _env.getBuild().observeInput(c, shared_from_this(), _content_version, t);

  // Return the metadata version
  return _content_version;
}

/// Check to see if this artifact's content matches a known version
void FileArtifact::match(shared_ptr<Command> c, shared_ptr<ContentVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getContent(c, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    _env.getBuild().observeMismatch(c, shared_from_this(), observed, expected);
  }
}

/// Apply a new content version to this artifact
void FileArtifact::apply(shared_ptr<Command> c,
                         shared_ptr<Reference> ref,
                         shared_ptr<ContentVersion> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  _content_version = writing;

  // Report the output to the build
  _env.getBuild().observeOutput(c, shared_from_this(), _content_version);
}
