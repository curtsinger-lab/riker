#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>

#include "util/serializer.hh"

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;
using std::weak_ptr;

class Command;
class Reference;

using Metadata = struct stat;
using Fingerprint = struct stat;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  /// Record a version that was not created by any command
  Version() = default;

  /// Record a version that was created by a specified command
  Version(shared_ptr<Command> creator) : _creator(creator) {}

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const { return _creator.lock(); }

  /// Is this version saved in a way that allows us to reproduce it?
  bool isSaved() const { return false; }

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

  /// Compare the fingerprint for this version to another version
  bool fingerprintMatch(shared_ptr<Version> other) const;

  /// Check if this version has been accessed
  bool isAccessed() const { return _accessed; }

  /// Mark this version as accessed
  void setAccessed() { _accessed = true; }

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
  /// Get saved metadata for this version
  optional<Metadata> getMetadata() const { return _metadata; }

  /// Get the saved fingerprint for this version
  optional<Fingerprint> getFingerprint() const { return nullopt; }

 private:
  /// The command that created this version
  weak_ptr<Command> _creator;

  /// Saved metadata for this version
  optional<Metadata> _metadata;

  /// Transient: has this version been accessed?
  bool _accessed = false;

  // Specify fields for serialization
  SERIALIZE(_creator, _metadata);
};
