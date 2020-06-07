#include "Artifact.hh"

#include <memory>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "build/Build.hh"
#include "build/Env.hh"
#include "data/Command.hh"
#include "data/IR.hh"
#include "data/Version.hh"
#include "ui/options.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::nullopt;
using std::shared_ptr;

Artifact::Artifact(Env& env, bool committed, shared_ptr<Version> v) : _env(env) {
  appendVersion(v, committed);
  _metadata_version = v;
}

// Check if this artifact can be restored to the filesystem
bool Artifact::canCommit() const {
  // Get an iterator to the first uncommitted version
  auto iter = _versions.begin();
  std::advance(iter, _committed_versions);

  // If any of the remaining versons cannot be committed, the artifact cannot be committed
  while (iter != _versions.end()) {
    auto v = *iter;
    if (!v->canCommit()) return false;
    iter++;
  }

  return true;
}

void Artifact::commit(shared_ptr<Reference> ref) {
  // Get an iterator to the first uncommitted version
  auto iter = _versions.begin();
  std::advance(iter, _committed_versions);

  // Commit the remaining versions in sequence
  while (iter != _versions.end()) {
    auto v = *iter;
    FAIL_IF(!v->canCommit()) << "Attempted to commit an unsaved version";
    v->commit(ref);
    _committed_versions++;
    iter++;
  }
}

// Check this artifact's contents and metadata against the filesystem state
void Artifact::checkFinalState(shared_ptr<Reference> ref) {
  // If this artifact is committed to the filesystem, we already know it matches
  if (isCommitted()) return;

  // Create a version that represents the on-disk contents reached through this reference
  auto v = make_shared<Version>();
  v->saveMetadata(ref);

  // Report a metadata mismatch if necessary
  if (!_metadata_version->metadataMatch(v)) {
    _env.getBuild().observeFinalMetadataMismatch(shared_from_this(), _metadata_version, v);
  }
}

// Save metadata for the latest version of this artifact
void Artifact::saveMetadata(shared_ptr<Reference> ref) {
  _metadata_version->saveMetadata(ref);
}

// Command c accesses this artifact's metadata
// Return the version it observes, or nullptr if no check is necessary
shared_ptr<Version> Artifact::accessMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref) {
  // Do we need to log this access?
  if (_metadata_filter.readRequired(c, ref)) {
    // Record the read
    _metadata_filter.readBy(c);

    // Yes. Notify the build and return the version
    _env.getBuild().observeMetadataInput(c, shared_from_this(), _metadata_version);
    return _metadata_version;

  } else {
    // No. Just return nullptr
    return nullptr;
  }
}

// Command c sets the metadata for this artifact.
// Return the version created by this operation, or nullptr if no new version is necessary.
shared_ptr<Version> Artifact::setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                          shared_ptr<Version> v) {
  // If no version was provided, the new version will represent what is currently on disk
  if (!v) {
    // Is a new version even required?
    if (!_metadata_filter.writeRequired(c, ref)) return nullptr;

    // Create a version to track the new on-disk state
    v = make_shared<Version>();

    // Append the new version and mark it as committed
    appendVersion(v, true);
  } else {
    // Append the un-committed version
    appendVersion(v, false);
  }

  // Track the new metadata version
  _metadata_version = v;

  // Record the write
  _metadata_filter.writtenBy(c, ref);

  // Inform the environment of this output
  _env.getBuild().observeMetadataOutput(c, shared_from_this(), _metadata_version);

  // Return the new metadata version
  return v;
}

void Artifact::appendVersion(shared_ptr<Version> v, bool committed) {
  if (committed) {
    FAIL_IF(!isCommitted()) << "Artifact is not fully committed: " << _committed_versions << ", "
                            << _versions.size();
    _committed_versions++;
  }

  _versions.push_back(v);
  v->identify(this);
}

// Record that the given command issued a read
void Artifact::AccessFilter::readBy(shared_ptr<Command> reader) {
  _accessed = true;
}

// Check if a read access must be logged separately, or if it can be safely ignored
bool Artifact::AccessFilter::readRequired(shared_ptr<Command> reader, shared_ptr<Reference> ref) {
  // We can skip reads if:
  // 1. The reader is the last writer
  // 2. The read is issued with the same reference as the last write
  if (!options::ignore_self_reads) return true;
  if (reader != _last_writer) return true;
  if (ref != _write_ref) return true;

  return false;
}

// Record that the given command issued a write
void Artifact::AccessFilter::writtenBy(shared_ptr<Command> writer, shared_ptr<Reference> ref) {
  _last_writer = writer;
  _write_ref = ref;
  _accessed = false;
}

// Check if a write access must be logged separately, or if it can be safely ignored
bool Artifact::AccessFilter::writeRequired(shared_ptr<Command> writer, shared_ptr<Reference> ref) {
  // We can skip writes if:
  // 1. The writer is the same as the last writer
  // 2. The write is issued with the same reference
  // 3. There have been no accesses since the last write (the intermediate state was not seen)
  if (!options::combine_writes) return true;
  if (writer != _last_writer) return true;
  if (ref != _write_ref) return true;
  if (_accessed) return true;

  return false;
}
