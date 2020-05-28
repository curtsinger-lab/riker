#include "Artifact.hh"

#include <memory>

#include "data/Command.hh"
#include "ui/options.hh"

using std::make_shared;
using std::shared_ptr;

/// Get the path to this artifact, if it has one.
/// This is ONLY useful for pretty printing artifacts; the actual path(s) to this artifact can
/// change during a build.
optional<string> Artifact::getPath() const {
  if (auto a = dynamic_pointer_cast<Access>(_ref)) {
    return a->getPath();
  } else {
    return nullopt;
  }
}

/**
 * Command c accesses the metadata for this artifact.
 * Return the version c will observe, or nullptr if this version has already been accessed.
 */
shared_ptr<Version> Artifact::accessMetadata(shared_ptr<Command> c) {
  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && getLatestVersion()->getCreator() == c) return nullptr;

  // Add this check to the set of metadata checks. If the check is not new, we can return.
  /*if (options::skip_repeat_checks && !c->checkMetadataRequired(ref, a->getLatestVersion())) {
    return nullptr;
  }*/

  // Get the latest version, mark it as accessed, and return it
  auto v = getLatestVersion();
  v->setAccessed();
  return v;
}

/**
 * Command c accesses the contents for this artifact.
 * Return the version c will observe, or nullptr if this version has already been accessed.
 */
shared_ptr<Version> Artifact::accessContents(shared_ptr<Command> c) {
  // When the optimization is enabled, we can assume that a command sees its own writes without
  // having to record the dependency. This is always safe.
  if (options::ignore_self_reads && getLatestVersion()->getCreator() == c) return nullptr;

  // Add this check to the set of contents checks. If the check is not new, we can return.
  /*if (options::skip_repeat_checks && !checkContentsRequired(ref, a->getLatestVersion())) {
    return;
  }*/

  // Get the latest version, mark it as accessed, and return it
  auto v = getLatestVersion();
  v->setAccessed();
  return v;
}

/**
 * Command c sets the metadata for this artifact.
 * Return the version created by this operation, or nullptr if no new version is necessary.
 */
shared_ptr<Version> Artifact::setMetadata(shared_ptr<Command> c) {
  // We cannot do write-combining on metadata updates because any access to a path could depend on
  // an update to the metadata of any artifact along that path (e.g. /, /foo, /foo/bar, ...)

  // Create a new version
  auto v = make_shared<Version>(c);
  _versions.push_back(v);
  return v;
}

/**
 * Command c sets the contents of this artifact.
 * Return the version created by this operation, or nullptr if no new version is necessary.
 */
shared_ptr<Version> Artifact::setContents(shared_ptr<Command> c) {
  // If this command created the last version, and no other command has accessed it, we can
  // combine the updates into a single update. That means we don't need to tag a new version.
  if (options::combine_writes && getLatestVersion()->getCreator() == c &&
      !getLatestVersion()->isAccessed()) {
    return nullptr;
  }

  // If we reach this point, the command is creating a new version of the artifact

  // Create a new version of the artifact
  auto v = make_shared<Version>(c);
  _versions.push_back(v);
  return v;
}
