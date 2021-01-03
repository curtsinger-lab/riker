#include "PipeArtifact.hh"

#include <memory>

#include "runtime/Build.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/PipeVersion.hh"

using std::shared_ptr;

class Command;
class Env;

// Can a specific version of this artifact be committed?
bool PipeArtifact::canCommit(shared_ptr<MetadataVersion> v) const noexcept {
  return v->isCommitted() || v->canCommit();
}

// Can a specific version of this artifact be committed?
bool PipeArtifact::canCommit(shared_ptr<ContentVersion> v) const noexcept {
  return v->isCommitted() || v->canCommit();
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

// Mark all versions of this artifact as committed
void PipeArtifact::setCommitted() noexcept {
  if (_last_read) _last_read->setCommitted(true);
  for (auto& write : _writes) {
    write->setCommitted(true);
  }

  Artifact::setCommitted();
}

// A traced command is about to close a reference to this artifact
void PipeArtifact::beforeClose(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Is the command closing the last writable reference to this pipe?
  if (c->currentRun()->getRef(ref)->getFlags().w) {
    auto final_write = make_shared<PipeCloseVersion>();
    final_write->createdBy(c->currentRun());
    _writes.push_back(final_write);
    appendVersion(final_write);
  }
}

// A traced command just read from this artifact
void PipeArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The reading command depends on the last read
  if (_last_read) c->currentRun()->addInput(shared_from_this(), _last_read, InputType::Accessed);

  // Create a new version to track this read
  auto read_version = make_shared<PipeReadVersion>(_writes);

  LOG(artifact) << "Creating pipe read version " << read_version;

  // The reader updates this pipe with the read version
  build.traceUpdateContent(c, ref, read_version);

  // The command should expect to read an equivalent version on future builds
  build.traceMatchContent(c, ref, read_version);
}

// A trace command just wrote to this artifact
void PipeArtifact::beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Create a new version
  auto writing = make_shared<PipeWriteVersion>();

  // The command writes this version to the pipe
  build.traceUpdateContent(c, ref, writing);
}

// Get this pipe's content without creating any dependencies
shared_ptr<ContentVersion> PipeArtifact::peekContent() noexcept {
  if (_last_read) {
    return _last_read;
  } else {
    return make_shared<PipeReadVersion>(_writes);
  }
}

// Check to see if this artifact's content matches a known version
void PipeArtifact::matchContent(const shared_ptr<Command>& c,
                                Scenario scenario,
                                shared_ptr<ContentVersion> expected) noexcept {
  // If nothing has been read from this pipe, there can be no match
  if (!_last_read) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _last_read);
    c->currentRun()->inputChanged(shared_from_this(), nullptr, expected, scenario);
  }

  // The command depends on the last read version
  c->currentRun()->addInput(shared_from_this(), _last_read, InputType::Accessed);

  // Compare the current content version to the expected version
  if (!_last_read->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _last_read);
    // Report the mismatch
    c->currentRun()->inputChanged(shared_from_this(), _last_read, expected, scenario);
  }
}

// Apply a new content version to this artifact
void PipeArtifact::updateContent(const shared_ptr<Command>& c,
                                 shared_ptr<ContentVersion> writing) noexcept {
  // Append the new version to the list of versions
  appendVersion(writing);

  // Report the output to the build
  c->currentRun()->addOutput(shared_from_this(), writing);

  // Is the written version a pipe write or pipe read?
  if (auto read = writing->as<PipeReadVersion>()) {
    // Set the last read version and clear the list of writes
    _last_read = read;

    // The reading command depends on all writes since the last read
    for (auto write : _writes) {
      c->currentRun()->addInput(shared_from_this(), write, InputType::Accessed);
    }

    // Clear the list of unread writes
    _writes.clear();

  } else if (auto write = writing->as<PipeWriteVersion>()) {
    // Add this write to the list of un-read writes
    _writes.push_back(write);

  } else if (auto close = writing->as<PipeCloseVersion>()) {
    // Add the close operation to the list of un-read writes
    _writes.push_back(close);

  } else {
    FAIL << "Unsupported pipe version type " << writing;
  }
}

int PipeArtifact::getFD(AccessFlags flags) noexcept {
  ASSERT((flags.r && !flags.w) || (flags.w && !flags.r))
      << "Invalid access flags for pipe: " << flags;

  if (!_fds.has_value()) {
    int pipefds[2];
    int rc = pipe2(pipefds, O_CLOEXEC);
    FAIL_IF(rc != 0) << "Failed to create pipe";
    _fds = tuple{pipefds[0], pipefds[1]};
    LOG(exec) << "Created pipe with read fd " << pipefds[0] << " and write fd " << pipefds[1];
  }

  auto [read_fd, write_fd] = _fds.value();

  if (flags.r) {
    ASSERT(read_fd >= 0) << "Attempted to return invalid pipe file descriptor from " << this;
    // Mark the read fd as invalid so it cannot be returned a second time
    _fds = tuple{-1, write_fd};
    return read_fd;

  } else {
    ASSERT(write_fd >= 0) << "Attempted to return invalid pipe file descriptor from " << this;
    // Mark teh write fd as invalid so it cannot be returned a second time
    _fds = tuple{read_fd, -1};
    return write_fd;
  }
}
