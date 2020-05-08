#include "Version.hh"

#include <memory>

#include <sys/stat.h>

#include "data/IR.hh"

using std::dynamic_pointer_cast;
using std::shared_ptr;

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