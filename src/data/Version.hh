#pragma once

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <sys/stat.h>

#include "util/serializer.hh"

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;

class Reference;

using Metadata = struct stat;
using Fingerprint = struct stat;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  /// Record a version that was not created by any command
  Version() = default;

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Is this version saved in a way that allows us to reproduce it?
  bool isSaved() const;

  /// Do we have saved metadata for this version?
  bool hasMetadata() const { return _metadata.has_value(); }

  /// Save the metadata for this version
  void saveMetadata(shared_ptr<Reference> ref);

  /// Compare metadata for this version to another version
  bool metadataMatch(shared_ptr<Version> other) const;

  /// Do we have a fingerprint for the contents of this version?
  bool hasFingerprint() const { return false; }

  /// Save a fingerprint of this version's contents
  void saveFingerprint(shared_ptr<Reference> ref);

  /// Compare the contents of this version to another version
  bool contentsMatch(shared_ptr<Version> other) const;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) {
    /*if (auto p = v.getPath(); p.has_value()) {
      return o << "[Artifact " << p.value() << "]@v" << v.getVersionNumber();
    } else {
      return o << "[Artifact]@v" << v.getVersionNumber();
    }*/
    // TODO: Print something useful here
    return o << "VERSION";
  }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) { return o << *v; }

 private:
  /// Saved metadata for this version
  optional<Metadata> _metadata;

  // Specify fields for serialization
  SERIALIZE(_metadata);
};
