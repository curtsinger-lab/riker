#include "Version.hh"

#include <memory>

#include <sys/stat.h>

#include "data/IR.hh"
#include "ui/log.hh"

using std::dynamic_pointer_cast;
using std::shared_ptr;

bool Version::metadataMatch(shared_ptr<Version> other) const {
  // Get metadata from both versions
  auto m1 = this->getMetadata();
  auto m2 = other->getMetadata();

  // We need metadata for both versions to compare
  if (!m1.has_value() || !m2.has_value()) return false;

  // We only compare uid, gid, and mode (which covers both type and permissions)
  if (m1.value().st_uid != m2.value().st_uid) {
    LOG << "uid mismatch";
    return false;
  }

  if (m1.value().st_gid != m2.value().st_gid) {
    LOG << "gid mismatch";
    return false;
  }

  if (m1.value().st_mode != m2.value().st_mode) {
    LOG << "mode mismatch";
    return false;
  }

  // That's it. Metadata must match
  return true;
}

/// Equality function for timespec structs
bool operator==(const timespec& t1, const timespec& t2) {
  return t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec;
}

bool Version::fingerprintMatch(shared_ptr<Version> other) const {
  // Get metadata from both versions
  auto m1 = this->getMetadata();
  auto m2 = other->getMetadata();

  // We need metadata from both versions to compare
  if (!m1.has_value() || !m2.has_value()) {
    LOG << "Fingerprint mismatch: missing metadata";
    return false;
  }

  // Compare mtimes
  if (m1.value().st_mtim == m2.value().st_mtim) {
    return true;
  } else {
    LOG << "Fingerprint mismatch: different mtimes";
    LOG << "  " << this << ": " << m1.value().st_mtim.tv_sec << ", " << m1.value().st_mtim.tv_nsec;
    LOG << "  " << other << ": " << m2.value().st_mtim.tv_sec << ", " << m2.value().st_mtim.tv_nsec;
    return false;
  }
}

void Version::saveMetadata(shared_ptr<Reference> ref) {
  auto a = dynamic_pointer_cast<Access>(ref);
  if (!a) {
    _metadata = nullopt;
    return;
  }

  struct stat statbuf;
  int rc;
  if (a->getFlags().nofollow) {
    rc = lstat(a->getPath().c_str(), &statbuf);
  } else {
    rc = stat(a->getPath().c_str(), &statbuf);
  }

  if (rc == 0) {
    _metadata = statbuf;
  }
}

void Version::saveFingerprint(shared_ptr<Reference> ref) {
  saveMetadata(ref);
}
