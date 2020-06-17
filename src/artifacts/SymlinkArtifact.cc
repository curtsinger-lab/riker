#include "SymlinkArtifact.hh"

#include <memory>

#include "build/Build.hh"
#include "versions/SymlinkVersion.hh"

using std::shared_ptr;

SymlinkArtifact::SymlinkArtifact(Env& env,
                                 bool committed,
                                 shared_ptr<MetadataVersion> mv,
                                 shared_ptr<SymlinkVersion> sv) noexcept :
    Artifact(env, committed, mv) {
  appendVersion(sv);
  _symlink_version = sv;
  _symlink_committed = committed;
}

const shared_ptr<SymlinkVersion>& SymlinkArtifact::readlink(shared_ptr<Command> c) noexcept {
  _env.getBuild().observeInput(c, shared_from_this(), _symlink_version);
  return _symlink_version;
}
