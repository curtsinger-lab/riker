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
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::ostream;
using std::shared_ptr;
using std::tuple;

/******* Emulation *******/

void Pipe::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.pipe(c, as<Pipe>());
}

void Access::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.access(c, _path, _flags, _base, as<Access>());
}

void MetadataMatch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateMetadataMatch(c, as<MetadataMatch>());
}

void ContentsMatch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateContentsMatch(c, as<ContentsMatch>());
}

void SymlinkMatch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateSymlinkMatch(c, as<SymlinkMatch>());
}

void SetMetadata::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateSetMetadata(c, as<SetMetadata>());
}

void SetContents::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateSetContents(c, as<SetContents>());
}

void Link::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateLink(c, as<Link>());
}

void Unlink::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateUnlink(c, as<Unlink>());
}

void Launch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.emulateLaunch(c, as<Launch>());
}

void Join::emulate(shared_ptr<Command> c, Build& build) noexcept {
  build.join(c, _cmd, _exit_status, as<Join>());
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

/// Print a CONTENTS_MATCH predicate
ostream& SymlinkMatch::print(ostream& o) const noexcept {
  return o << "SYMLINK_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a SET_METADATA action
ostream& SetMetadata::print(ostream& o) const noexcept {
  return o << "SET_METADATA(" << getReference()->getName() << ", " << _version << ")";
}

/// Print a SET_CONTENTS action
ostream& SetContents::print(ostream& o) const noexcept {
  return o << "SET_CONTENTS(" << getReference()->getName() << ", " << _version << ")";
}

/// Print a LINK action
ostream& Link::print(ostream& o) const noexcept {
  return o << "LINK(" << _ref->getName() << ", " << _entry << ", " << _target->getName() << ")";
}

/// Print an UNLINK action
ostream& Unlink::print(ostream& o) const noexcept {
  return o << "UNLINK(" << _ref->getName() << ", " << _entry << ")";
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
