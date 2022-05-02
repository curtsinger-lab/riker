#include "SpecialArtifact.hh"

#include <filesystem>
#include <memory>
#include <optional>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "util/options.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SpecialVersion.hh"

using std::make_shared;
using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

SpecialArtifact::SpecialArtifact(MetadataVersion mv, bool always_changed) noexcept :
    Artifact(mv), _always_changed(always_changed) {
  // Create an initial committed version
  auto cv = make_shared<SpecialVersion>(!always_changed);
  _content.update(cv);
  appendVersion(cv);
}

/// Revert this artifact to its committed state
void SpecialArtifact::rollback() noexcept {
  _content.rollback();
  Artifact::rollback();
}

// Get a file descriptor for this artifact
int SpecialArtifact::getFD(AccessFlags flags) noexcept {
  if (_fd >= 0) {
    return _fd;
  } else {
    return Artifact::getFD(flags);
  }
}

// Set the file descriptor for this artifact
void SpecialArtifact::setFD(int fd) noexcept {
  _fd = fd;
}

// Commit the content of this artifact to the filesystem
void SpecialArtifact::commitContentTo(fs::path path) noexcept {}

/// Commit a link to this artifact at the given path
void SpecialArtifact::commitLink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we find one, return.
  auto iter = _committed_links.find(entry);
  if (iter != _committed_links.end()) return;

  WARN << "Committing a link of a special artifact is not supported";

  // Record the committed link and return
  _committed_links.emplace_hint(iter, entry);
  return;
}

/// Commit an unlink of this artifact at the given path
void SpecialArtifact::commitUnlink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we don't find one, return immediately.
  auto iter = _committed_links.find(entry);
  if (iter == _committed_links.end()) return;

  WARN << "Committing an unlink of a special artifact is not supported";

  // Remove the committed link and return
  _committed_links.erase(iter);
  return;
}

/// Compare all final versions of this artifact to the filesystem state
void SpecialArtifact::checkFinalState(fs::path path) noexcept {
  // If _always_changed is true then any uncommitted state is treated as a change
  if (_always_changed && _content.isUncommitted()) {
    auto [version, weak_creator] = _content.getUncommitted();
    auto creator = weak_creator.lock();

    auto [committed_version, _] = _content.getCommitted();

    creator->outputChanged(shared_from_this(), committed_version, version);
  }
}

/// Commit any pending versions and save fingerprints for this artifact
void SpecialArtifact::applyFinalState(fs::path path) noexcept {
  // Just set everything as committed
  _content.setCommitted();

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(path);
}

/// A traced command is about to (possibly) read from this artifact
void SpecialArtifact::beforeRead(Build& build,
                                 const IRSource& source,
                                 const shared_ptr<Command>& c,
                                 Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void SpecialArtifact::afterRead(Build& build,
                                const IRSource& source,
                                const shared_ptr<Command>& c,
                                Ref::ID ref) noexcept {
  // The command now depends on the content of this special artifact
  build.matchContent(source, c, Scenario::Build, ref, getContent(c));
}

/// A traced command is about to (possibly) write to this artifact
void SpecialArtifact::beforeWrite(Build& build,
                                  const IRSource& source,
                                  const shared_ptr<Command>& c,
                                  Ref::ID ref) noexcept {
  // The command now depends on the content of this special artifact
  build.matchContent(source, c, Scenario::Build, ref, getContent(c));
}

/// A traced command just wrote to this artifact
void SpecialArtifact::afterWrite(Build& build,
                                 const IRSource& source,
                                 const shared_ptr<Command>& c,
                                 Ref::ID ref) noexcept {
  // Create a new version
  auto writing = make_shared<SpecialVersion>(!_always_changed);

  // The command wrote to this special artifact
  build.updateContent(source, c, ref, writing);
}

/// A traced command is about to truncate this artifact to length 0
void SpecialArtifact::beforeTruncate(Build& build,
                                     const IRSource& source,
                                     const shared_ptr<Command>& c,
                                     Ref::ID ref) noexcept {
  // Do nothing before a truncate
}

/// A trace command just truncated this artifact to length 0
void SpecialArtifact::afterTruncate(Build& build,
                                    const IRSource& source,
                                    const shared_ptr<Command>& c,
                                    Ref::ID ref) noexcept {
  // The command wrote an empty content version to this artifact
  auto written = make_shared<SpecialVersion>(!_always_changed);

  build.updateContent(source, c, ref, written);
}

// Get this artifact's content version
shared_ptr<ContentVersion> SpecialArtifact::getContent(const shared_ptr<Command>& c) noexcept {
  auto [version, writer] = _content.getLatest();
  if (c) c->addContentInput(shared_from_this(), version, writer.lock());
  return version;
}

/// Check to see if this artifact's content matches a known version
void SpecialArtifact::matchContent(const shared_ptr<Command>& c,
                                   Scenario scenario,
                                   shared_ptr<ContentVersion> expected) noexcept {
  // Get the current content version
  auto observed = getContent(c);

  // If this artifact is treated as always-changed, report a change to the reader
  if (_always_changed) {
    c->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

/// Apply a new content version to this artifact
void SpecialArtifact::updateContent(const shared_ptr<Command>& c,
                                    shared_ptr<ContentVersion> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  auto sv = writing->as<SpecialVersion>();

  FAIL_IF(!sv) << "Attempted to apply version " << writing << " to special artifact " << this;

  // Update the content
  _content.update(c, sv);

  // Report the output to the build
  c->addContentOutput(shared_from_this(), writing);
}
