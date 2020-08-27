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

/****** Resolve References ******/

// Emulate a special reference
Resolution SpecialRef::resolve(shared_ptr<Command> c,
                               Build& build,
                               shared_ptr<Resolve> result,
                               bool committed) noexcept {
  return build.resolveSpecialRef(c, _entity, result, committed);
}

// Emulate a pipe reference
Resolution PipeRef::resolve(shared_ptr<Command> c,
                            Build& build,
                            shared_ptr<Resolve> result,
                            bool committed) noexcept {
  return build.resolvePipeRef(c, result, committed);
}

// Emulate an anonymous file reference
Resolution FileRef::resolve(shared_ptr<Command> c,
                            Build& build,
                            shared_ptr<Resolve> result,
                            bool committed) noexcept {
  return build.resolveFileRef(c, _mode, result, committed);
}

// Emulate a symlink reference
Resolution SymlinkRef::resolve(shared_ptr<Command> c,
                               Build& build,
                               shared_ptr<Resolve> result,
                               bool committed) noexcept {
  return build.resolveSymlinkRef(c, _target, result, committed);
}

// Emulate a dir reference
Resolution DirRef::resolve(shared_ptr<Command> c,
                           Build& build,
                           shared_ptr<Resolve> result,
                           bool committed) noexcept {
  return build.resolveDirRef(c, _mode, result, committed);
}

// Emulate a path access reference
Resolution PathRef::resolve(shared_ptr<Command> c,
                            Build& build,
                            shared_ptr<Resolve> result,
                            bool committed) noexcept {
  return build.resolvePathRef(c, _base, _path, _flags, result, committed);
}

/****** Emulate IR Steps ******/

// Emulate a Resolve step
void Resolve::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateResolve(c, _ref, this->as<Resolve>());
}

// Emulate an ExpectResult predicate
void ExpectResult::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateExpectResult(c, _ref, _expected, this->as<ExpectResult>());
}

// Emulate a MatchMetadata predicate
void MatchMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateMatchMetadata(c, _ref, _version, this->as<MatchMetadata>());
}

// Emulate a MatchContent predicate
void MatchContent::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateMatchContent(c, _ref, _version, this->as<MatchContent>());
}

// Emulate an UpdateMetadata IR step
void UpdateMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateUpdateMetadata(c, _ref, _version, this->as<UpdateMetadata>());
}

// Emulate an UpdateContent IR step
void UpdateContent::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateUpdateContent(c, _ref, _version, this->as<UpdateContent>());
}

// Emulate a launch action
void Launch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateLaunch(c, _cmd, this->as<Launch>());
}

// Emulate a join action
void Join::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateJoin(c, _cmd, _exit_status, this->as<Join>());
}

// Emulate an exit action
void Exit::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateExit(c, _exit_status, this->as<Exit>());
}
