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

/****** Emulate IR Steps ******/

// Emulate a SpecialRef step
void SpecialRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateSpecialRef(c, _entity, _output);
}

// Emulate a PipeRef step
void PipeRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulatePipeRef(c, _output);
}

// Emulate a FileRef step
void FileRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateFileRef(c, _mode, _output);
}

// Emulate a SymlinkRef step
void SymlinkRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateSymlinkRef(c, _target, _output);
}

// Emulate a DirRef step
void DirRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateDirRef(c, _mode, _output);
}

// Emulate a PathRef step
void PathRef::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulatePathRef(c, _base, _path, _flags, _output);
}

// Emulate an ExpectResult predicate
void ExpectResult::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateExpectResult(c, _ref, _expected);
}

// Emulate a MatchMetadata predicate
void MatchMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateMatchMetadata(c, _ref, _version);
}

// Emulate a MatchContent predicate
void MatchContent::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateMatchContent(c, _ref, _version);
}

// Emulate an UpdateMetadata IR step
void UpdateMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateUpdateMetadata(c, _ref, _version);
}

// Emulate an UpdateContent IR step
void UpdateContent::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateUpdateContent(c, _ref, _version);
}

// Emulate a launch action
void Launch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateLaunch(c, _cmd);
}

// Emulate a join action
void Join::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateJoin(c, _cmd, _exit_status);
}

// Emulate an exit action
void Exit::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateExit(c, _exit_status);
}
