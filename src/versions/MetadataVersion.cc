#include "MetadataVersion.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>

#include "core/IR.hh"

using std::nullopt;
using std::shared_ptr;

bool MetadataVersion::checkAccess(shared_ptr<Artifact> artifact, AccessFlags flags) noexcept {
  // If we don't have a saved value for metadata but this version is committed, go save it now
  if (!_metadata.has_value() && isCommitted()) {
    // Get a path to the artifact
    auto path = artifact->getCommittedPath();
    ASSERT(path.has_value()) << "Committed artifact has no path";
    save(path.value());
  }

  // Make sure we have metadata to check access against
  ASSERT(_metadata.has_value()) << "Trying to check access against unsaved metadata";

  auto mode = _metadata.value().mode;

  // TODO: Currently checking against the current user and group. This should use the user and
  // group of the running process/command. These should probably be encoded with the reference.
  // We're also not checking against supplementary groups.

  if (_metadata.value().uid == getuid()) {
    if (flags.r && !(mode & S_IRUSR)) return false;
    if (flags.w && !(mode & S_IWUSR)) return false;
    if (flags.x && !(mode & S_IXUSR)) return false;
    return true;

  } else if (_metadata.value().gid == getgid()) {
    if (flags.r && !(mode & S_IRGRP)) return false;
    if (flags.w && !(mode & S_IWGRP)) return false;
    if (flags.x && !(mode & S_IXGRP)) return false;
    return true;

  } else {
    if (flags.r && !(mode & S_IROTH)) return false;
    if (flags.w && !(mode & S_IWOTH)) return false;
    if (flags.x && !(mode & S_IXOTH)) return false;
    return true;
  }
}

// Save metadata
void MetadataVersion::save(fs::path path) noexcept {
  if (_metadata.has_value()) return;

  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);

  if (rc == 0) _metadata = statbuf;
}

// Can this version be committed?
bool MetadataVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  return _metadata.has_value();
}

// Commit this version to the filesystem
void MetadataVersion::commit(fs::path path, bool commit_permissions) noexcept {
  if (isCommitted()) return;
  ASSERT(canCommit()) << "Attempted to commit unsaved version";

  int rc = ::lchown(path.c_str(), _metadata.value().uid, _metadata.value().gid);
  FAIL_IF(rc != 0) << "Failed to commit owner and group to " << path << ": " << ERR;

  if (commit_permissions) {
    rc = ::chmod(path.c_str(), _metadata.value().mode);
    FAIL_IF(rc != 0) << "Failed to commit permissions to " << path << ": " << ERR;
  }

  // Mark this version as committed
  Version::setCommitted();
}
