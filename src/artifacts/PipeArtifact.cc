#include "PipeArtifact.hh"

#include <memory>

#include "runtime/Build.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"

using std::shared_ptr;

class Command;
class Env;

// Check if a written pipe version matches another version
bool PipeWriteVersion::matches(shared_ptr<Version> other) const noexcept {
  return other->as<PipeWriteVersion>().get() == this;
}

// Check if a read pipe version matches another version
bool PipeReadVersion::matches(shared_ptr<Version> other) const noexcept {
  // Cast the other version to a PipeReadVersion
  auto r = other->as<PipeReadVersion>();
  if (!r) return false;

  // Are the two versions the same instance?
  if (r.get() == this) return true;

  // Do the two sets of reads observe the same number of writes?
  if (_observed.size() != r->_observed.size()) return false;

  // Walk through and compare all the observed writes
  auto self_iter = _observed.begin();
  auto other_iter = r->_observed.begin();

  while (self_iter != _observed.end()) {
    auto self_write = *self_iter;
    auto other_write = *other_iter;
    if (!self_write->matches(other_write)) return false;
    self_iter++;
    other_iter++;
  }

  return true;
}

// Can a specific version of this artifact be committed?
bool PipeArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  return v->isCommitted();
}

// Can this artifact be fully committed?
bool PipeArtifact::canCommitAll() const noexcept {
  if (!_metadata_version->isCommitted()) return false;
  if (_last_read && !_last_read->isCommitted()) return false;
  for (auto write : _writes) {
    if (!write->isCommitted()) return false;
  }
  return true;
}

// Command c requires that this artifact exists in its current state. Create dependency edges.
void PipeArtifact::mustExist(Build& build, shared_ptr<Command> c) noexcept {
  build.observeInput(c, shared_from_this(), _metadata_version, InputType::Exists);

  // If there is a last read version, it must exist
  if (_last_read) build.observeInput(c, shared_from_this(), _last_read, InputType::Exists);

  for (auto write : _writes) {
    build.observeInput(c, shared_from_this(), write, InputType::Exists);
  }
}

// Mark all versions of this artifact as committed
void PipeArtifact::setCommitted() noexcept {
  if (_last_read) _last_read->setCommitted(true);
  for (auto& write : _writes) {
    write->setCommitted(true);
  }

  Artifact::setCommitted();
}

// A traced command just read from this artifact
void PipeArtifact::afterRead(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<RefResult> ref) noexcept {
  // The reading command depends on the last read
  if (_last_read) build.observeInput(c, shared_from_this(), _last_read, InputType::Accessed);

  // Create a new version to track this read
  auto read_version = make_shared<PipeReadVersion>(_writes);

  LOG(artifact) << "Creating pipe read version " << read_version;

  // The reader updates this pipe with the read version
  build.traceUpdateContent(c, ref, read_version);

  // The command should expect to read an equivalent version on future builds
  build.traceMatchContent(c, ref, read_version);
}

// A trace command just wrote to this artifact
void PipeArtifact::beforeWrite(Build& build,
                               shared_ptr<Command> c,
                               shared_ptr<RefResult> ref) noexcept {
  // Create a new version
  auto writing = make_shared<PipeWriteVersion>();

  // The command writes this version to the pipe
  build.traceUpdateContent(c, ref, writing);
}

// Get this pipe's content without creating any dependencies
shared_ptr<Version> PipeArtifact::peekContent() noexcept {
  return make_shared<PipeReadVersion>(_writes);
}

// Check to see if this artifact's content matches a known version
void PipeArtifact::matchContent(Build& build,
                                shared_ptr<Command> c,
                                Scenario scenario,
                                shared_ptr<Version> expected) noexcept {
  // If nothing has been read from this pipe, there can be no match
  if (!_last_read) build.observeMismatch(c, scenario, shared_from_this(), nullptr, expected);

  // The command depends on the last read version
  build.observeInput(c, shared_from_this(), _last_read, InputType::Accessed);

  // Compare the current content version to the expected version
  if (!_last_read->matches(expected)) {
    // Report the mismatch
    build.observeMismatch(c, scenario, shared_from_this(), _last_read, expected);
  }
}

// Apply a new content version to this artifact
void PipeArtifact::updateContent(Build& build,
                                 shared_ptr<Command> c,
                                 shared_ptr<Version> writing) noexcept {
  // Append the new version to the list of versions
  appendVersion(writing);

  // Report the output to the build
  build.observeOutput(c, shared_from_this(), writing);

  // Is the written version a pipe write or pipe read?
  if (auto read = writing->as<PipeReadVersion>()) {
    // Set the last read version and clear the list of writes
    _last_read = read;

    // The reading command depends on all writes since the last read
    for (auto write : _writes) {
      build.observeInput(c, shared_from_this(), write, InputType::Accessed);
    }

    // Clear the list of unread writes
    _writes.clear();

  } else if (auto write = writing->as<PipeWriteVersion>()) {
    // Add this write to the list of un-read writes
    _writes.push_back(write);

  } else {
    FAIL << "Unsupported pipe version type " << writing;
  }
}
