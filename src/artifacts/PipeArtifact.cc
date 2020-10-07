#include "PipeArtifact.hh"

#include <memory>

#include "runtime/Build.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"

using std::shared_ptr;

class Command;
class Env;

PipeArtifact::PipeArtifact(shared_ptr<Env> env,
                           shared_ptr<MetadataVersion> mv,
                           shared_ptr<FileVersion> cv) noexcept :
    Artifact(env, mv) {
  appendVersion(cv);
  _content_version = cv;
}

// Can a specific version of this artifact be committed?
bool PipeArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  return v->isCommitted();
}

// Can this artifact be fully committed?
bool PipeArtifact::canCommitAll() const noexcept {
  return _metadata_version->isCommitted() && _content_version->isCommitted();
}

// Command c requires that this artifact exists in its current state. Create dependency edges.
void PipeArtifact::mustExist(Build& build, shared_ptr<Command> c) noexcept {
  build.observeInput(c, shared_from_this(), _metadata_version, InputType::Exists);
  build.observeInput(c, shared_from_this(), _content_version, InputType::Exists);
}

// Mark all versions of this artifact as committed
void PipeArtifact::setCommitted() noexcept {
  _content_version->setCommitted(true);
  Artifact::setCommitted();
}

// A traced command just read from this artifact
void PipeArtifact::afterRead(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<RefResult> ref) noexcept {
  // The current content version is an input to command c
  build.observeInput(c, shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

// A traced command is about to (possibly) write to this artifact
void PipeArtifact::beforeWrite(Build& build,
                               shared_ptr<Command> c,
                               shared_ptr<RefResult> ref) noexcept {
  // The content version is an input to command c
  build.observeInput(c, shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

// A trace command just wrote to this artifact
void PipeArtifact::afterWrite(Build& build,
                              shared_ptr<Command> c,
                              shared_ptr<RefResult> ref) noexcept {
  // Create a new version
  auto writing = make_shared<FileVersion>();

  // The command wrote to this file
  build.traceUpdateContent(c, ref, writing);
}

// Check to see if this artifact's content matches a known version
void PipeArtifact::matchContent(Build& build,
                                shared_ptr<Command> c,
                                shared_ptr<Version> expected) noexcept {
  // The content version is an input to command c
  build.observeInput(c, shared_from_this(), _content_version, InputType::Accessed);

  // Compare the current content version to the expected version
  if (!_content_version->matches(expected)) {
    // Report the mismatch
    build.observeMismatch(c, shared_from_this(), _content_version, expected);
  }
}

// Apply a new content version to this artifact
void PipeArtifact::updateContent(Build& build,
                                 shared_ptr<Command> c,
                                 shared_ptr<Version> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  _content_version = writing->as<FileVersion>();

  FAIL_IF(!_content_version) << "Attempted to apply version " << writing << " to file artifact "
                             << this;

  // Report the output to the build
  build.observeOutput(c, shared_from_this(), writing);
}
