#include "Artifact.hh"

#include <sys/stat.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

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
ArtifactVersion Artifact::getLatestVersion() {
  // Tag an initial version of each artifact on demand
  if (_versions.size() == 0) tagNewVersion();
  return ArtifactVersion(shared_from_this(), _versions.size() - 1);
}

// Save a new version of an artifact
ArtifactVersion Artifact::tagNewVersion() {
  _versions.push_back(Artifact::VersionData());
  return ArtifactVersion(shared_from_this(), _versions.size() - 1);
}

// Save the metadata for a version
void ArtifactVersion::saveMetadata() {
  FAIL_IF(!_artifact) << "Attempted to save metadata for version of null artifact";
  FAIL_IF(_index >= _artifact->_versions.size())
      << "Attempted to save metadata for invalid version index";
  FAIL_IF(_index != _artifact->_versions.size() - 1)
      << "Attempted to save metadata for a version after it has been overwritten";

  // Get the version data
  Artifact::VersionData& v = _artifact->_versions[_index];

  // Only save metadata if we don't have it already
  if (!v.metadata.has_value()) {
    struct stat s;
    if (stat(_artifact->_path.c_str(), &s) == 0) {
      v.metadata = s;
    } else {
      WARN << "Failed to stat artifact " << _artifact;
    }
  }
}

// Check if a version has saved metadata
bool ArtifactVersion::hasMetadata() const {
  return _artifact->_versions[_index].metadata.has_value();
}

// Check if a version has a fingerprint
bool ArtifactVersion::hasFingerprint() const {
  return _artifact->_versions[_index].fingerprint.has_value();
}

// Check if the contents of an artifact version have been saved
bool ArtifactVersion::hasSavedContents() const {
  return _artifact->_versions[_index].saved.has_value();
}

// Get a list of versions for this artifact
list<ArtifactVersion> Artifact::getVersions() {
  list<ArtifactVersion> result;
  for (size_t i = 0; i < _versions.size(); i++) {
    result.push_back(ArtifactVersion(shared_from_this(), i));
  }
  return result;
}
