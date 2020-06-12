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
  uint16_t mode;
  uid_t uid;
  gid_t gid;

  /// Default constructor for deserialization
  Metadata() noexcept = default;

  /// Create a Metadata object from stat data
  Metadata(struct stat& s) noexcept : mode(s.st_mode), uid(s.st_uid), gid(s.st_gid) {}

  /// Compare to another Metadata instance
  bool operator==(const Metadata& other) const noexcept {
    return std::tie(mode, uid, gid) == std::tie(other.mode, other.uid, other.gid);
  }

  SERIALIZE(mode, uid, gid);
};

class MetadataVersion final : public Version {
 public:
  /// Create a new metadata version with unknown metadata
  MetadataVersion() noexcept = default;

  /// Cerate a new metadata version with existing metadata
  MetadataVersion(Metadata&& m) noexcept : _metadata(m) {}

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept final { return "metadata"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const noexcept final { return _metadata.has_value(); }

  /// Save this version so it can be committed later
  virtual void save(shared_ptr<Reference> ref) noexcept final;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const noexcept final;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const noexcept final { return _metadata.has_value(); }

  /// Save a fingerprint of this version
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept final { save(ref); }

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept final;

 private:
  optional<Metadata> _metadata;

  SERIALIZE(BASE(Version), _metadata);
};