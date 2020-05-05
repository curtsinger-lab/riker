#include "Version.hh"

#include <memory>

#include <sys/stat.h>

#include "core/IR.hh"

using std::dynamic_pointer_cast;
using std::shared_ptr;

// Check if an artifact is a system file
bool Version::isSystemFile() const {
  if (!_path.has_value()) return false;

  for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
    // Check if the path begins with one of our prefixes.
    // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
    if (_path.value().rfind(p, 0) != string::npos) return true;
  }
  return false;
}

shared_ptr<Version> Version::getFirstVersion() {
  auto current = shared_from_this();
  while (current->_previous) {
    current = current->_previous;
  }
  return current;
}

// Save the metadata for a version
void Version::saveMetadata(shared_ptr<Reference> ref) {
  auto a = dynamic_pointer_cast<Access>(ref);
  if (!a) return;

  // Only save metadata if we don't have it already
  if (!_metadata.has_value()) {
    struct stat s;
    if (stat(a->getPath().c_str(), &s) == 0) {
      _metadata = s;
    } else {
      // WARN << "Failed to stat artifact " << _artifact;
    }
  }
}

// Save a fingerprint of this file
void Version::saveFingerprint(shared_ptr<Reference> ref) {
  // Just use metadata as a fingerprint (mtime) for now
  saveMetadata(ref);
}