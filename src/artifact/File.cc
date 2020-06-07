#include "File.hh"

#include <memory>
#include <string>

#include "artifact/Artifact.hh"
#include "build/Build.hh"
#include "data/Version.hh"

using std::shared_ptr;
using std::string;

FileArtifact::FileArtifact(Env& env, bool committed, shared_ptr<Version> v) :
    Artifact(env, committed, v) {
  _content_version = v;
}

void FileArtifact::checkFinalState(shared_ptr<Reference> ref) {
  // If this artifact is committed to the filesystem, we already know it matches
  if (isCommitted()) return;

  // Create a version that represents the on-disk contents reached through this reference
  auto v = make_shared<Version>();
  v->saveFingerprint(ref);

  // Report a content mismatch if necessary
  if (!_content_version->contentsMatch(v)) {
    _env.getBuild().observeFinalContentMismatch(shared_from_this(), _content_version, v);
  }

  // Have the artifact check final state as well
  Artifact::checkFinalState(ref);
}

// Save a fingerprint of the contents of the latest version of this artifact
void FileArtifact::saveFingerprint(shared_ptr<Reference> ref) {
  _content_version->saveFingerprint(ref);
}

// Command c accesses this artifact's contents
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<Version> FileArtifact::accessContents(shared_ptr<Command> c, shared_ptr<Reference> ref) {
  // Do we need to log this access?
  if (_content_filter.readRequired(c, ref)) {
    // Record the read
    _content_filter.readBy(c);

    // Yes. Notify the build and return the version
    _env.getBuild().observeContentInput(c, shared_from_this(), _content_version);
    return _content_version;

  } else {
    // No. Just return nullptr
    return nullptr;
  }
}

// Command c sets the contents of this artifact to an existing version. Used during emulation.
shared_ptr<Version> FileArtifact::setContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                              shared_ptr<Version> v) {
  // If no version was provided, the new version will represent what is currently on disk
  if (!v) {
    // Is a new version even required?
    if (!_content_filter.writeRequired(c, ref)) return nullptr;

    // Create a version to track the new on-disk state
    v = make_shared<Version>();

    // Append the new version and mark it as committed
    appendVersion(v, true);
  } else {
    // Append the new uncommitted version
    appendVersion(v, false);
  }

  // Track the new content version
  _content_version = v;

  // Record the write
  _content_filter.writtenBy(c, ref);

  // Inform the environment of this output
  _env.getBuild().observeContentOutput(c, shared_from_this(), _content_version);

  // Return the new metadata version
  return v;
}
