#include "MetadataVersion.hh"

#include <memory>
#include <optional>

#include "core/IR.hh"

using std::nullopt;
using std::shared_ptr;

bool MetadataVersion::checkAccess(AccessFlags flags) noexcept {
  if (_metadata.has_value()) {
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

  } else {
    WARN << "Checking access against unknown metadata";
    // TODO: Find cases that trigger this path and modify them to pre-populate metadata.
    // For now, just say it's okay.
    return true;
  }
}

// Save metadata
void MetadataVersion::save(shared_ptr<Reference> ref) noexcept {
  if (_metadata.has_value()) return;

  // Check the reference type
  if (auto a = ref->as<Access>()) {
    // Get stat data and save it
    auto [info, rc] = a->lstat();
    if (rc == SUCCESS) _metadata = info;
  }
}

// Can this version be committed?
bool MetadataVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  return _metadata.has_value();
}

// Commit this version to the filesystem
void MetadataVersion::commit(shared_ptr<Reference> ref) noexcept {
  if (isCommitted()) return;
  ASSERT(canCommit()) << "Attempted to commit unsaved version";
  // TODO: Commit metadata state

  // Mark this version as committed
  Version::setCommitted();
}
