#include "IR.hh"

#include <map>
#include <memory>
#include <ostream>
#include <tuple>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "artifacts/Pipe.hh"
#include "build/Build.hh"
#include "build/BuildObserver.hh"
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
  resolvesTo(build.getEnv().getPipe(c), SUCCESS);
}

void Access::resolve(shared_ptr<Command> c, Build& build) noexcept {
  // The base reference must be resolved already.
  ASSERT(!_base || _base->isResolved()) << "Attempted to resolve reference " << this
                                        << " without first resolving base reference " << _base;

  // Get the relative part of the path and start an iterator
  auto path = getRelativePath();
  auto path_iter = path.begin();

  // If the relative path is empty, we can stop immediately
  if (path_iter == path.end()) {
    auto a = _base->getArtifact();
    if (!a->checkAccess(_flags)) {
      resolvesTo(nullptr, EACCES);
    } else {
      resolvesTo(a, SUCCESS);
    }
    return;
  }

  auto dir = _base->getArtifact();
  auto dir_path = _base->getFullPath();

  // Keep a human-readable name for the artifact based on the name assigned to the base directory
  fs::path dir_name = dir->getName();

  // Loop forever. The exit condition is inside the loop
  while (true) {
    // Get the current entry name, then advance the path iterator
    auto entry = *path_iter;
    path_iter++;

    // Update the human-readable name we will assign to the resolved artifact
    dir_name /= entry;

    // Are we processing the final entry in the path?
    if (path_iter == path.end()) {
      // Yes. Make the access with the flags for this reference
      auto [artifact, rc] = dir->getEntry(dir_path, entry);

      // If there's an artifact, set its name
      if (artifact) artifact->setName(dir_name.lexically_normal());

      if (_flags.create && _flags.exclusive && rc == SUCCESS) {
        // The access was required to create the file, but it already exists
        resolvesTo(nullptr, EEXIST);
      } else if (_flags.create && rc == ENOENT) {
        // No entry exists, but it is being created by this access
        artifact = build.getEnv().createFile(getFullPath(), c, _flags);
        dir->setEntry(entry, artifact);
        // TODO: what happens if open creates a read-only file, but the open call is accessing it in
        // writable mode? Is the file created?
        resolvesTo(artifact, SUCCESS);

      } else if (rc != SUCCESS) {
        // The access failed for some other reason
        resolvesTo(nullptr, rc);
      } else if (!artifact->checkAccess(_flags)) {
        // The entry exists, but the requested access is not allowed
        resolvesTo(nullptr, EACCES);
      } else {
        // Resolution succeeded
        resolvesTo(artifact, SUCCESS);
      }

      // All done
      return;

    } else {
      // This is NOT the last entry along the path. First, make sure we have permission to access
      // entries in this directory (requires execute permissions)
      if (!dir->checkAccess(AccessFlags{.x = true})) {
        resolvesTo(nullptr, EACCES);
        return;
      }

      // Now ask the directory for the entry we want
      auto [artifact, rc] = dir->getEntry(dir_path, entry);

      // If the access failed, record the result and return
      if (rc != SUCCESS) {
        resolvesTo(artifact, rc);
        return;
      }

      // Otherwise, move on to the next directory and update our record of its path
      dir = artifact;
      dir_path /= entry;
    }
  }
}

/******* Emulation *******/

void Pipe::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // Resolve the reference
  resolve(c, build);
  if (getResult() != getExpectedResult()) build.observeCommandChange(c, shared_from_this());
}

void Access::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // Resolve the reference
  resolve(c, build);
  if (getResult() != getExpectedResult()) build.observeCommandChange(c, shared_from_this());
}

void MetadataMatch::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // If the reference does not resolve, report a change
  if (_ref->getResult() != SUCCESS) {
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
  if (_ref->getResult() != SUCCESS) {
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
  if (_ref->getResult() != SUCCESS) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  _ref->getArtifact()->setMetadata(c, _ref, _version);
}

void SetContents::emulate(shared_ptr<Command> c, Build& build) noexcept {
  // If the reference did not resolve, report a change
  if (_ref->getResult() != SUCCESS) {
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

tuple<struct stat, int> Access::stat() const noexcept {
  struct stat statbuf;
  int rc = ::stat(getFullPath().c_str(), &statbuf);
  return {statbuf, rc};
}

tuple<struct stat, int> Access::lstat() const noexcept {
  struct stat statbuf;
  int rc = ::lstat(getFullPath().c_str(), &statbuf);
  return {statbuf, rc};
}

int Access::access() const noexcept {
  auto [access_mode, access_flags] = _flags.toAccess();
  return faccessat(AT_FDCWD, getFullPath().c_str(), access_mode, access_flags);
}

/******************** Print Methods ********************/

// Set up a map from return codes to names
static map<int8_t, string> errors = {
    {SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
    {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
    {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}, {ENOTDIR, "ENOTDIR"}};

/// Print a PIPE reference
ostream& Pipe::print(ostream& o) const noexcept {
  return o << getName() << " = PIPE() -> " << errors[getExpectedResult()];
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
  o << "LAUNCH(" << _cmd << ", [";
  bool first = true;
  for (const auto& entry : _cmd->getInitialFDs()) {
    if (!first) o << ", ";
    first = false;
    o << entry.second.getReference()->getName();
  }
  return o << "])";
}

// Print a join action
ostream& Join::print(ostream& o) const noexcept {
  return o << "JOIN(" << _cmd << ", " << _exit_status << ")";
}
