#include "File.hh"

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Build.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::shared_ptr;
using std::string;

FileArtifact::FileArtifact(Env& env, bool committed, shared_ptr<MetadataVersion> mv,
                           shared_ptr<ContentVersion> cv) :
    Artifact(env, committed, mv) {
  appendVersion(cv);
  _content_version = cv;
  _content_committed = committed;
}

// Check if the latest version of this artifact is saved
bool FileArtifact::isSaved() const {
  return _content_version->isSaved() && Artifact::isSaved();
}

// Save a copy of the latest version of this artifact
void FileArtifact::save(const shared_ptr<Reference>& ref) {
  // Save the content
  _content_version->save(ref);

  // Delegate metadata saving to the artifact
  Artifact::save(ref);
}

// Check if the latest version of this artifact are committed to disk
bool FileArtifact::isCommitted() const {
  return _content_committed && Artifact::isCommitted();
}

// Commit the latest version of this artifact to the filesystem
void FileArtifact::commit(const shared_ptr<Reference>& ref) {
  if (!_content_committed) {
    ASSERT(_content_version->isSaved()) << "Attempted to commit unsaved version";

    // Commit the file contents
    _content_version->commit(ref);
    _content_committed = true;
  }

  // Delegate metadata commits to the artifact
  Artifact::commit(ref);
}

// Check if we have a fingerprint for the latest version of this artifact
bool FileArtifact::hasFingerprint() const {
  return _content_version->hasFingerprint() && Artifact::hasFingerprint();
}

// Save a fingerprint for the latest version of this artifact
void FileArtifact::fingerprint(const shared_ptr<Reference>& ref) {
  _content_version->fingerprint(ref);
  Artifact::fingerprint(ref);
}

void FileArtifact::checkFinalState(const shared_ptr<Reference>& ref) {
  // If we already know the file contents are committed, we can skip content checks
  if (!_content_committed) {
    // Create a version that represents the on-disk contents reached through this reference
    auto v = make_shared<ContentVersion>();
    v->fingerprint(ref);

    // Report a content mismatch if necessary
    if (!_content_version->matches(v)) {
      _env.getBuild().observeFinalMismatch(shared_from_this(), _content_version, v);
    } else {
      // Since the contents match what is on disk, we can treat this artifact as if it was committed
      _content_committed = true;
    }
  }

  // Check metadata in the top-level artifact
  Artifact::checkFinalState(ref);
}

// Command c accesses this artifact's contents
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<ContentVersion> FileArtifact::accessContents(const shared_ptr<Command>& c,
                                                        const shared_ptr<Reference>& ref) {
  _content_accessed = true;

  // Yes. Notify the build and return the version
  _env.getBuild().observeInput(c, shared_from_this(), _content_version);
  return _content_version;
}

// Command c sets the contents of this artifact to an existing version. Used during emulation.
shared_ptr<ContentVersion> FileArtifact::setContents(const shared_ptr<Command>& c,
                                                     const shared_ptr<Reference>& ref,
                                                     const shared_ptr<ContentVersion>& v) {
  // If no version was provided, the new version will represent what is currently on disk
  if (!v) {
    // Create a version to track the new on-disk state
    _content_version = make_shared<ContentVersion>();

    // Append the new version and mark it as committed
    appendVersion(_content_version);
    _content_committed = true;

  } else {
    // Adopt v as the new content version
    _content_version = v;

    // Append the new uncommitted version
    appendVersion(_content_version);
    _content_committed = false;
  }

  // Update creator, ref, and accessed tracking info
  _content_creator = c;
  _content_accessed = false;

  // Inform the environment of this output
  _env.getBuild().observeOutput(c, shared_from_this(), _content_version);

  // Return the new metadata version
  return _content_version;
}
