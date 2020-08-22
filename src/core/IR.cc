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

// Emulate a pipe reference
void Pipe::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.pipe(c, as<Pipe>());
}

// Emulate an anonymous file reference
void File::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.file(c, _mode, as<File>());
}

// Emulate a symlink reference
void Symlink::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.symlink(c, _target, as<Symlink>());
}

// Emulate a dir reference
void Dir::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.dir(c, _mode, as<Dir>());
}

// Emulate a path access reference
void Access::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.access(c, _base, _path, _flags, as<Access>());
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

/******************* Access Methods ********************/

int Access::open() const noexcept {
  auto [open_flags, open_mode] = _flags.toOpen();
  return ::open(getFullPath().c_str(), open_flags, open_mode);
}
