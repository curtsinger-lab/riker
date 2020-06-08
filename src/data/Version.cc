#include "Version.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifact/Artifact.hh"
#include "data/IR.hh"
#include "data/Metadata.hh"
#include "util/log.hh"

using std::dynamic_pointer_cast;
using std::nullopt;
using std::shared_ptr;

bool Version::canCommit() const {
  // Empty files can be recreated
  if (_metadata.has_value() && _metadata.value().empty) {
    return true;
  }

  // We're not saving any other files at this point
  return false;
}

void Version::commit(shared_ptr<Reference> ref) {
  if (auto a = dynamic_pointer_cast<Access>(ref)) {
    if (_metadata.has_value() && _metadata.value().empty) {
      int fd = a->open();
      FAIL_IF(fd < 0) << "Failed to commit empty file version: " << ERR;
      close(fd);
    }
  }
}

bool Version::metadataMatch(shared_ptr<Version> other) const {
  // A version always matches itself
  if (this == other.get()) return true;

  // We need metadata for both versions to compare
  if (!_metadata.has_value() || !other->_metadata.has_value()) return false;

  // Compare metadata
  if (_metadata.value() == other->_metadata.value()) {
    // Metadata matches. Share the identity of both versions, and return true
    identify(other);
    return true;
  } else {
    return false;
  }
}

/// Equality function for timespec structs
bool operator==(const timespec& t1, const timespec& t2) {
  return t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec;
}

bool Version::contentsMatch(shared_ptr<Version> other) const {
  // A version always matches itself
  if (this == other.get()) return true;

  // Get metadata from both versions
  auto& m1 = _metadata;
  auto& m2 = other->_metadata;

  // We need metadata from both versions to compare
  if (!m1.has_value() || !m2.has_value()) {
    return false;
  }

  // Compare mtimes
  if (m1.value().mtime == m2.value().mtime) {
    // The other version matched, so copy the identity between them
    identify(other);
    return true;
  } else {
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
  if (a->stat(&statbuf) == 0) {
    _metadata = statbuf;
  }
}

void Version::saveFingerprint(shared_ptr<Reference> ref) {
  saveMetadata(ref);
}

void Version::identify(const Artifact* a) const {
  _identity = string("[") + a->getName() + " v" + std::to_string(a->getVersionCount() - 1) + "]";
}

void Version::identify(shared_ptr<Version> other) const {
  if (_identity.has_value()) {
    other->_identity = _identity;
  } else if (other->_identity.has_value()) {
    _identity = other->_identity;
  }
}

ostream& operator<<(ostream& o, const Version& v) {
  return o << v._identity.value_or("[Unknown Version]");
}
