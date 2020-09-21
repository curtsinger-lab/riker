#include "MetadataVersion.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>

#include "artifacts/Artifact.hh"

using std::nullopt;
using std::shared_ptr;

bool MetadataVersion::checkAccess(shared_ptr<Artifact> artifact, AccessFlags flags) noexcept {
  // If we don't have a saved value for metadata but this version is committed, go save it now
  if (!_metadata.has_value() && isCommitted()) {
    // Get a path to the artifact, including only committed paths
    auto path = artifact->getPath(false);
    ASSERT(path.has_value()) << "Committed artifact has no path";
    save(path.value());
  }

  // Make sure we have metadata to check access against
  ASSERT(_metadata.has_value()) << "Trying to check access against unsaved metadata";

  auto mode = _metadata.value().mode;

  // TODO: Currently checking against the current user and group. This should use the user and
  // group of the running process/command. These should probably be encoded with the reference.
  // We're also not checking against supplementary groups.

  // get user/group for artifact
  auto a_uid = _metadata.value().uid;
  auto a_gid = _metadata.value().gid;

  // compute capabilities
  bool can_r = (a_uid == getuid() && (mode & S_IRUSR)) || (a_gid == getgid() && (mode & S_IRGRP)) ||
               (mode & S_IROTH);
  bool can_w = (a_uid == getuid() && (mode & S_IWUSR)) || (a_gid == getgid() && (mode & S_IWGRP)) ||
               (mode & S_IWOTH);
  bool can_x = (a_uid == getuid() && (mode & S_IXUSR)) || (a_gid == getgid() && (mode & S_IXGRP)) ||
               (mode & S_IXOTH);

  // is the user requesting a capability that they do not have?
  if (flags.r && !can_r) return false;
  if (flags.w && !can_w) return false;
  if (flags.x && !can_x) return false;
  return true;
}

// Save metadata
void MetadataVersion::save(fs::path path) noexcept {
  if (_metadata.has_value()) return;

  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);

  if (rc == 0) _metadata = statbuf;
}

// Get the mode field from this metadata version
mode_t MetadataVersion::getMode() const noexcept {
  ASSERT(_metadata.has_value()) << "Cannot get mode field for an unsaved metadata version";
  return _metadata.value().mode;
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

// Apply a MetadataVersion version to an artifact
void MetadataVersion::applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept {
  a->updateMetadata(b, c, this->as<MetadataVersion>());
}
