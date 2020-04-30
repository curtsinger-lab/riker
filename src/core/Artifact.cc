#include "Artifact.hh"

#include <memory>

#include <sys/stat.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

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
  // Tag an initial version of each artifact on demand
  if (_versions.size() == 0) tagNewVersion();
  return _versions.back();
}

// Save a new version of an artifact
shared_ptr<Version> Artifact::tagNewVersion(shared_ptr<Command> creator) {
  auto v = make_shared<Version>(shared_from_this(), _versions.size(), creator);
  _versions.push_back(v);
  return v;
}

// Save the metadata for a version
void Version::saveMetadata() {
  FAIL_IF(!_artifact) << "Attempted to save metadata for version of null artifact";
  FAIL_IF(_index >= _artifact->_versions.size())
      << "Attempted to save metadata for invalid version index";
  FAIL_IF(_index != _artifact->_versions.size() - 1)
      << "Attempted to save metadata for a version after it has been overwritten";

  // Only save metadata if we don't have it already
  if (!_metadata.has_value()) {
    struct stat s;
    if (stat(_artifact->_path.c_str(), &s) == 0) {
      _metadata = s;
    } else {
      // WARN << "Failed to stat artifact " << _artifact;
    }
  }
}

// Save a fingerprint of this file
void Version::saveFingerprint() {
  // Just use metadata as a fingerprint (mtime) for now
  saveMetadata();
}
