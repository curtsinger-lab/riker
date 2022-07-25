#pragma once

#include <filesystem>
#include <memory>
#include <ostream>

#include <sys/stat.h>
#include <sys/types.h>

#include "util/stats.hh"
#include "versions/Version.hh"

namespace fs = std::filesystem;

class AccessFlags;
class Artifact;
class Command;

class MetadataVersion : public Version {
 public:
  /// The type of a MetadataVersion ID
  using ID = uint32_t;

  /// Create a new metadata version
  MetadataVersion(uid_t uid, gid_t gid, mode_t mode) noexcept : _uid(uid), _gid(gid), _mode(mode) {}

  /// Create a new metadata version from a stat struct
  MetadataVersion(const struct stat& data) noexcept :
      MetadataVersion(data.st_uid, data.st_gid, data.st_mode) {}

  /// Create a new metadata version by changing the owner and/or group in this one
  MetadataVersion chown(uid_t user, gid_t group) const noexcept;

  /// Create a new metadata version by changing the mode bits in this one
  MetadataVersion chmod(mode_t mode) const noexcept;

  /// Check if a given access is allowed by the mode bits in this metadata record
  bool checkAccess(AccessFlags flags) noexcept;

  /// Get the mode field from this metadata version
  mode_t getMode() const noexcept;

  /// Commit this version to the filesystem
  void commit(fs::path path) noexcept;

  /// Compare this version to another version
  bool matches(MetadataVersion other) const noexcept;

  /// Get the name for the type of version this is
  virtual std::string getTypeName() const noexcept override { return "metadata"; }

  /// Print this metadata version
  virtual std::ostream& print(std::ostream& o) const noexcept override;

 private:
  /// The user id for this metadata version
  uid_t _uid;

  /// The group id for this metadata version
  gid_t _gid;

  /// The file mode bits for this metadata version
  mode_t _mode;
};

template <>
struct fmt::formatter<MetadataVersion> : ostream_formatter {};
