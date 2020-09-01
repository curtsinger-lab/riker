#include "IR.hh"

#include <map>
#include <memory>
#include <ostream>
#include <tuple>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "build/Build.hh"
#include "build/BuildObserver.hh"
#include "build/Env.hh"
#include "core/Command.hh"
#include "util/log.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::ostream;
using std::shared_ptr;
using std::tuple;

/******* Emulation *******/

// Emulate a special reference
void SpecialRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.specialRef(c, as<SpecialRef>());
}

// Emulate a pipe reference
void PipeRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.pipeRef(c, as<PipeRef>());
}

// Emulate an anonymous file reference
void FileRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.fileRef(c, _mode, as<FileRef>());
}

// Emulate a symlink reference
void SymlinkRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.symlinkRef(c, _target, as<SymlinkRef>());
}

// Emulate a dir reference
void DirRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.dirRef(c, _mode, as<DirRef>());
}

// Emulate a path access reference
void PathRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.pathRef(c, _base, _path, _flags, as<PathRef>());
}

// Emulate an ExpectResult predicate
void ExpectResult::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.expectResult(c, _ref, _expected, as<ExpectResult>());
}

// Emulate a MatchMetadata predicate
void MatchMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.matchMetadata(c, _ref, _version, as<MatchMetadata>());
}

// Emulate a MatchContent predicate
void MatchContent::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.matchContent(c, _ref, _version, as<MatchContent>());
}

// Emulate an UpdateMetadata IR step
void UpdateMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.updateMetadata(c, _ref, _version, as<UpdateMetadata>());
}

// Emulate an UpdateContent IR step
void UpdateContent::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.updateContent(c, _ref, _version, as<UpdateContent>());
}

// Emulate a launch action
void Launch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.launch(c, _cmd, as<Launch>());
}

// Emulate a join action
void Join::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.join(c, _cmd, _exit_status, as<Join>());
}

// Emulate an exit action
void Exit::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.exit(c, _exit_status, as<Exit>());
}
