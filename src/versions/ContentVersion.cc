#include "ContentVersion.hh"

#include <memory>
#include <optional>

#include <unistd.h>

#include "core/IR.hh"
#include "util/log.hh"

using std::nullopt;
using std::shared_ptr;

// Is this version saved in a way that can be committed?
bool ContentVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  return _fingerprint.has_value() && _fingerprint.value().empty;
}

// Commit this version to the filesystem
void ContentVersion::commit(shared_ptr<Reference> ref) noexcept {
  if (isCommitted()) return;

  ASSERT(canCommit()) << "Attempted to commit unsaved version";

  if (auto a = ref->as<Access>()) {
    if (_fingerprint.has_value() && _fingerprint.value().empty) {
      int fd = a->open();
      FAIL_IF(fd < 0) << "Failed to commit empty file version: " << ERR;
      close(fd);
    }
  }

  // Mark this version as committed
  Version::setCommitted();
}

// Save a fingerprint of this version
void ContentVersion::fingerprint(shared_ptr<Reference> ref) noexcept {
  if (hasFingerprint()) return;

  // Check the reference type
  if (auto a = ref->as<Access>()) {
    // Get stat data and save it
    auto [info, rc] = a->lstat();
    if (rc == SUCCESS) _fingerprint = info;
  }
}
