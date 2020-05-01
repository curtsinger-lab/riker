#include "Artifact.hh"

#include <memory>

#include <sys/stat.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::shared_ptr;

// Check if an artifact is a system file
bool Artifact::isSystemFile() const {
  for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
    // Check if the path begins with one of our prefixes.
    // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
    if (_path.rfind(p, 0) != string::npos) return true;
  }
  return false;
}

// Get a reference to the latest version of an artifact
shared_ptr<Version> Artifact::getLatestVersion() {
  if (!_latest) tagNewVersion();
  return _latest;
}

// Save a new version of an artifact
shared_ptr<Version> Artifact::tagNewVersion(shared_ptr<Command> creator) {
  if (!_latest) {
    _latest = make_shared<Version>(shared_from_this(), creator);
    _latest->_index = 0;
  } else {
    auto v = make_shared<Version>(shared_from_this(), creator);
    v->_previous = _latest;
    v->_index = _latest->_index + 1;
    _latest->_next = v;
    _latest = v;
  }

  return _latest;
}

list<shared_ptr<Version>> Artifact::getVersions() const {
  list<shared_ptr<Version>> result;
  auto current = _latest;
  while (current) {
    result.push_front(current);
    current = current->getPrevious();
  }
  return result;
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
