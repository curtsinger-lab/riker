#include "SocketArtifact.hh"

#include <cerrno>
#include <filesystem>
#include <memory>

#include "artifacts/DirArtifact.hh"
#include "data/AccessFlags.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/env.hh"
#include "util/log.hh"
#include "util/wrappers.hh"
#include "versions/ContentVersion.hh"
#include "versions/FileVersion.hh"

using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

SocketArtifact::SocketArtifact(MetadataVersion mv, shared_ptr<FileVersion> sv) noexcept :
    Artifact(mv) {
  appendVersion(sv);
}

/// Revert this artifact to its committed state
void SocketArtifact::rollback() noexcept {
  Artifact::rollback();
}

/// A traced command is about to (possibly) read from this artifact
void SocketArtifact::beforeRead(Build& build,
                                const IRSource& source,
                                const shared_ptr<Command>& c,
                                Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void SocketArtifact::afterRead(Build& build,
                               const IRSource& source,
                               const shared_ptr<Command>& c,
                               Ref::ID ref) noexcept {
  // The command now depends on the content of this file
  build.matchContent(source, c, Scenario::Build, ref, getContent(c));
}

/// A traced command is about to (possibly) write to this artifact
void beforeWrite(Build& build,
                 const IRSource& source,
                 const std::shared_ptr<Command>& c,
                 Ref::ID ref) noexcept {
  // TODO
}

/// A trace command just wrote to this artifact
void afterWrite(Build& build,
                const IRSource& source,
                const std::shared_ptr<Command>& c,
                Ref::ID ref) noexcept {
  // TODO
}
