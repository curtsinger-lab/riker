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

/// Resolve this reference on behalf of command c
Resolution Access::resolve(shared_ptr<Command> c, bool committed) noexcept {
  auto result = _base->getArtifact()->resolve(c, nullptr, _path.begin(), _path.end(), as<Access>(),
                                              committed);
  resolvesTo(result);
  return result;
}

/******* Emulation *******/

// Emulate a pipe reference
void Pipe::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.pipe(c, as<Pipe>());
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

// Emulate a match predicate
template <class VersionType>
void Match<VersionType>::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.match(c, _ref, _version, as<Match<VersionType>>());
}

// Explicitly instantiate Match for metadata versions
template void Match<MetadataVersion>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Explicitly instantiate Match for content versions
template void Match<FileVersion>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Explicitly instantiate Match for symlink versions
template void Match<SymlinkVersion>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Emulate an Apple action
template <class VersionType>
void Apply<VersionType>::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.apply(c, _ref, _version, as<Apply<VersionType>>());
}

// Explicitly instantiate Apply::emulate for metadata versions
template void Apply<MetadataVersion>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Explicitly instantiate Apple::emulate for content versions
template void Apply<FileVersion>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Explicitly instantiate Apple::emulate for directory link versions
template void Apply<AddEntry>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Explicitly instantiate Apple::emulate for directory unlink versions
template void Apply<RemoveEntry>::emulate(shared_ptr<Command> c, Build& build) noexcept;

// Emulate a launch action
void Launch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.launch(c, _cmd, as<Launch>());
}

// Emulate a join action
void Join::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.join(c, _cmd, _exit_status, as<Join>());
}

/******************* Access Methods ********************/

int Access::open() const noexcept {
  auto [open_flags, open_mode] = _flags.toOpen();
  return ::open(getFullPath().c_str(), open_flags, open_mode);
}
