#include "MetadataVersion.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>

#include "artifacts/Artifact.hh"
#include "util/wrappers.hh"

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

  // TODO: Currently checking against the dodo process's effective user and group(s). This check
  // should instead use the user and group(s) of the emulated process/command. Such attributes
  // should probably be stored within the reference.

  // if the user's effective uid is root, bypass all checks
  auto [gid, uid] = get_identity();
  if (uid == 0) return true;

  // user/group for artifact
  uid_t a_uid = _metadata.value().uid;
  gid_t a_gid = _metadata.value().gid;

  // Keep track of which permissions we still need to satisfy
  bool read_needed = flags.r;
  bool write_needed = flags.w;
  bool execute_needed = flags.x;

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to the owner
  if (uid == a_uid) {
    if (mode & S_IRUSR) read_needed = false;
    if (mode & S_IWUSR) write_needed = false;
    if (mode & S_IXUSR) execute_needed = false;
  }

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to the primary group
  if (gid == a_gid) {
    if (mode & S_IRGRP) read_needed = false;
    if (mode & S_IWGRP) write_needed = false;
    if (mode & S_IXGRP) execute_needed = false;
  }

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to world
  if (mode & S_IROTH) read_needed = false;
  if (mode & S_IWOTH) write_needed = false;
  if (mode & S_IXOTH) execute_needed = false;

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to the supplementary groups
  if (getgroups().find(a_gid) != getgroups().end()) {
    if (mode & S_IRGRP) read_needed = false;
    if (mode & S_IWGRP) write_needed = false;
    if (mode & S_IXGRP) execute_needed = false;
  }

  // Access is granted as long as all requested permissions were granted
  return !read_needed && !write_needed && !execute_needed;
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
