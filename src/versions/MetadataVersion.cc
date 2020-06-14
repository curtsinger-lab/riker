#include "MetadataVersion.hh"

#include <memory>
#include <optional>

#include "core/IR.hh"

using std::dynamic_pointer_cast;
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
  // We can only stat Access references. Try to cast.
  auto a = dynamic_pointer_cast<Access>(ref);
  if (!a) {
    // Not an Access reference, so we have no saved metadata
    _metadata = nullopt;
    return;
  }

  // Get stat data and save it
  auto [info, rc] = a->stat();
  if (rc == SUCCESS) _metadata = info;
}

// Commit this version to the filesystem
void MetadataVersion::commit(shared_ptr<Reference> ref) const noexcept {
  // TODO: Commit metadata state
  ASSERT(isSaved()) << "Attempted to commit unsaved version";
}

// Compare to another version
bool MetadataVersion::matches(shared_ptr<Version> other) const noexcept {
  // A version compares equal to itself, even if we have no saved metadata
  if (other.get() == this) return true;

  // If we have no saved metadata, we cannot find a match
  if (!_metadata.has_value()) return false;

  // Make sure the other version is a MetadataVersion
  auto v = dynamic_pointer_cast<MetadataVersion>(other);
  if (!v) return false;

  // Compare. If the other version does not have metadata, optional will compare false
  if (_metadata == v->_metadata) {
    identify(other);
    return true;
  } else {
    return false;
  }
}
