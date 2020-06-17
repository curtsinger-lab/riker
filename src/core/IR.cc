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
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::dynamic_pointer_cast;
using std::ostream;
using std::shared_ptr;
using std::tuple;

void Pipe::resolve(shared_ptr<Command> c, Build& build) noexcept {
  resolvesTo(build.getEnv().getPipe(c));
}

void Access::resolve(shared_ptr<Command> c, Build& build) noexcept {
  // The base reference must be resolved already.
  ASSERT(!_base || _base->isResolved()) << "Attempted to resolve reference " << this
                                        << " without first resolving base reference " << _base;

  fs::path base_path = _base->getFullPath();
  shared_ptr<Artifact> base_artifact = _base->getResolution();

  resolvesTo(build.getEnv().resolvePath(c, _path, _flags, base_path, base_artifact));
}

/******* Emulation *******/

void Pipe::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // Resolve the reference
  resolve(c, build);
  if (getResolution() != getExpectedResult()) build.observeCommandChange(c, shared_from_this());
}

void Access::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // Resolve the reference
  resolve(c, build);
  if (getResolution() != getExpectedResult()) build.observeCommandChange(c, shared_from_this());
}

void MetadataMatch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // If the reference does not resolve, report a change
  if (!_ref->getResolution()) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Get the latest metadata version. The returned version will be nullptr if no check is
  // necessary.
  const auto& v = _ref->getArtifact()->accessMetadata(c, _ref);

  // If a version was returned and it doesn't match the expected version, report a mismatch
  if (v && !v->matches(_version)) {
    build.observeMismatch(c, _ref->getArtifact(), v, _version);
  }
}

void ContentsMatch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // If the reference did not resolve, report a change
  if (!_ref->getResolution()) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Get the latest content version. The returned version will be nullptr if no check is
  // necessary.
  const auto& v = _ref->getArtifact()->accessContents(c, _ref);

  // If a version was returned and it doesn't match the expected version, report a mismatch
  if (v && !v->matches(_version)) {
    build.observeMismatch(c, _ref->getArtifact(), v, _version);
  }
}

void SetMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // If the reference did not resolve, report a change
  if (!_ref->getResolution()) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  _ref->getArtifact()->setMetadata(c, _ref, _version);
}

void SetContents::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // If the reference did not resolve, report a change
  if (!_ref->getResolution()) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  _ref->getArtifact()->setContents(c, _ref, _version);
}

void Launch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // Tell the build to launch the child command
  build.launch(c, _cmd);
}

void Join::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // Inform the build that it should join the child command
  build.join(_cmd);

  // Did the child command's exit status match the expected result?
  if (_cmd->getExitStatus() != _exit_status) {
    build.observeCommandChange(c, shared_from_this());
  }
}

/******************* Access Methods ********************/

int Access::open() const noexcept {
  auto [open_flags, open_mode] = _flags.toOpen();
  return ::open(getFullPath().c_str(), open_flags, open_mode);
}

tuple<struct stat, int> Access::lstat() const noexcept {
  struct stat statbuf;
  int rc = ::lstat(getFullPath().c_str(), &statbuf);
  return {statbuf, rc};
}

/******************** Print Methods ********************/

// Set up a map from return codes to names
static map<int8_t, string> errors = {
    {SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
    {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
    {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}, {ENOTDIR, "ENOTDIR"}};

/// Print a PIPE reference
ostream& Pipe::print(ostream& o) const noexcept {
  o << getName() << " = PIPE()";
  if (isResolved()) {
    // Print the artifact this pipe resolves to
    o << " -> " << getArtifact();
  } else {
    int rc = getResolution();
    o << " expect " << errors[rc];
  }
  return o;
}

/// Print an ACCESS reference
ostream& Access::print(ostream& o) const noexcept {
  return o << getName() << " = ACCESS(" << getFullPath() << ", [" << getFlags() << "]) -> "
           << errors[getExpectedResult()];
}

/// Print a METADATA_MATCH predicate
ostream& MetadataMatch::print(ostream& o) const noexcept {
  return o << "METADATA_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a CONTENTS_MATCH predicate
ostream& ContentsMatch::print(ostream& o) const noexcept {
  return o << "CONTENTS_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a SET_METADATA action
ostream& SetMetadata::print(ostream& o) const noexcept {
  return o << "SET_METADATA(" << getReference()->getName() << ", " << _version << ")";
}

/// Print a SET_CONTENTS action
ostream& SetContents::print(ostream& o) const noexcept {
  return o << "SET_CONTENTS(" << getReference()->getName() << ", " << _version << ")";
}

// Print a launch action
ostream& Launch::print(ostream& o) const noexcept {
  o << "LAUNCH(" << _cmd << ", fds={";
  bool first = true;
  for (const auto& entry : _cmd->getInitialFDs()) {
    if (!first) o << ", ";
    first = false;
    o << entry.first << ": " << entry.second.getReference()->getName();
  }
  return o << "})";
}

// Print a join action
ostream& Join::print(ostream& o) const noexcept {
  return o << "JOIN(" << _cmd << ", " << _exit_status << ")";
}
