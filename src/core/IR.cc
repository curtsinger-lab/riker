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
#include "artifacts/Symlink.hh"
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

  // Get the relative part of the path and start an iterator
  auto path = getRelativePath();
  auto path_iter = path.begin();

  // If the relative path is empty, we can stop immediately
  if (path_iter == path.end()) {
    auto a = _base->getArtifact();
    if (!a->checkAccess(_flags)) {
      resolvesTo(EACCES);
    } else {
      resolvesTo(a);
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
      auto res = dir->getEntry(dir_path, entry);

      // If there's an artifact, set its name
      if (res) res->setName(dir_name.lexically_normal());

      if (_flags.create && _flags.exclusive && res) {
        // The access was required to create the file, but it already exists
        resolvesTo(EEXIST);
        return;

      } else if (_flags.create && res == ENOENT) {
        // No entry exists, but it is being created by this access
        auto artifact = build.getEnv().createFile(getFullPath(), c, _flags);
        dir->setEntry(entry, artifact);
        // TODO: what happens if open creates a read-only file, but the open call is accessing it in
        // writable mode? Is the file created?
        resolvesTo(artifact);
        return;

      } else if (!res) {
        // The access failed for some other reason
        resolvesTo(res);
        return;

      } else if (!res->checkAccess(_flags)) {
        // The entry exists, but the requested access is not allowed
        resolvesTo(EACCES);
        return;

      } else {
        // Resolution succeeded
        resolvesTo(res);
        return;
      }

    } else {
      // This is NOT the last entry along the path. First, make sure we have permission to access
      // entries in this directory (requires execute permissions)
      if (!dir->checkAccess(AccessFlags{.x = true})) {
        resolvesTo(EACCES);
        return;
      }

      // Now ask the directory for the entry we want
      auto res = dir->getEntry(dir_path, entry);

      // If the access failed, record the result and return
      if (res != SUCCESS) {
        resolvesTo(res);
        return;
      } else if (auto symlink = res.as<SymlinkArtifact>()) {
        auto link_dest = symlink->readlink();
        shared_ptr<Access> link_ref;

        if (link_dest.is_relative()) {
          // Resolve the symlink relative to the directory that holds it
          auto dir_ref =
              make_shared<Access>(build.getEnv().getRootRef(), dir_path, AccessFlags{.x = true});
          dir_ref->resolvesTo(dir);

          link_ref = make_shared<Access>(dir_ref, link_dest, AccessFlags{.x = true});

        } else {
          // Resolve the symlink relative to the root directory
          link_ref =
              make_shared<Access>(build.getEnv().getRootRef(), link_dest, AccessFlags{.x = true});
        }

        // Resolve the symlink
        link_ref->resolve(c, build);

        // If the symlink did not resolve, return the error
        if (!link_ref->getResolution()) {
          resolvesTo(link_ref->getResolution());
          return;
        }

        // The link resolved, so the current directory is going to be the target of the link
        res = link_ref->getResolution();
      }

      // Otherwise, move on to the next directory and update our record of its path
      dir = res;
      dir_path /= entry;
    }
  }
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
