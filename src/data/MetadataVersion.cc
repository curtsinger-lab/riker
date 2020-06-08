#include "MetadataVersion.hh"

#include <memory>
#include <optional>

#include "data/IR.hh"

using std::dynamic_pointer_cast;
using std::nullopt;
using std::shared_ptr;

// Save metadata
void MetadataVersion::save(shared_ptr<Reference> ref) {
  // We can only stat Access references. Try to cast.
  auto a = dynamic_pointer_cast<Access>(ref);
  if (!a) {
    // Not an Access reference, so we have no saved metadata
    _metadata = nullopt;
    return;
  }

  // Get stat data and save it
  struct stat statbuf;
  if (a->stat(&statbuf) == 0) {
    _metadata = statbuf;
  }
}

// Commit this version to the filesystem
void MetadataVersion::commit(shared_ptr<Reference> ref) const {
  // TODO: Commit metadata state
  FAIL_IF(!isSaved()) << "Attempted to commit unsaved version";
}

// Compare to another version
bool MetadataVersion::matches(shared_ptr<Version> other) const {
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
