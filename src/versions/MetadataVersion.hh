#pragma once

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "util/serializer.hh"
#include "versions/Version.hh"

using std::optional;
using std::shared_ptr;
using std::string;

class Reference;

struct Metadata {
 public:
  uid_t uid;
  gid_t gid;
  uint16_t mode;

  /// Default constructor for deserialization
  Metadata() noexcept = default;

  /// Create a Metadata object from stat data
  Metadata(struct stat& s) noexcept : uid(s.st_uid), gid(s.st_gid), mode(s.st_mode) {}

  /// Create a Metadata object from specific uid, gid, and mode values
  Metadata(uid_t uid, gid_t gid, mode_t mode) : uid(uid), gid(gid), mode(mode) {}

  /// Compare to another Metadata instance
  bool operator==(const Metadata& other) const noexcept {
    if (uid != other.uid) return false;
    if (gid != other.gid) return false;
    if ((mode & S_IFMT) != (other.mode & S_IFMT)) return false;
    return true;
  }

  SERIALIZE(uid, gid, mode);
};

class MetadataVersion final : public Version {
 public:
  /// Create a new metadata version with unknown metadata
  MetadataVersion() noexcept = default;

  /// Cerate a new metadata version with existing metadata
  MetadataVersion(Metadata&& m) noexcept : _metadata(m) {}

  /// Check if a given access is allowed by the mode bits in this metadata record
  bool checkAccess(AccessFlags flags) noexcept;

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "metadata"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const noexcept override { return _metadata.has_value(); }

  /// Save this version so it can be committed later
  virtual void save(shared_ptr<Reference> ref) noexcept override;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const noexcept override;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const noexcept override { return _metadata.has_value(); }

  /// Save a fingerprint of this version
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept override { save(ref); }

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

 private:
  optional<Metadata> _metadata;

  SERIALIZE(BASE(Version), _metadata);
};