#pragma once

#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "data/AccessFlags.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::map;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;

class Ref;

inline static map<uint16_t, string> modes = {
    {S_IFSOCK, "sock"}, {S_IFLNK, "symlink"}, {S_IFREG, "file"}, {S_IFBLK, "blockdev"},
    {S_IFDIR, "dir"},   {S_IFCHR, "chardev"}, {S_IFIFO, "fifo"}};

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

  /// Print metadata
  friend ostream& operator<<(ostream& o, const Metadata& m) noexcept {
    o << "uid=" << m.uid << ", ";
    o << "gid=" << m.gid << ", ";
    o << "type=" << modes[m.mode & S_IFMT] << ", ";
    o << "perms=";
    o << (m.mode & S_IRUSR ? 'r' : '-');
    o << (m.mode & S_IWUSR ? 'w' : '-');
    o << (m.mode & S_IXUSR ? 'x' : '-');
    o << (m.mode & S_IRGRP ? 'r' : '-');
    o << (m.mode & S_IWGRP ? 'w' : '-');
    o << (m.mode & S_IXGRP ? 'x' : '-');
    o << (m.mode & S_IROTH ? 'r' : '-');
    o << (m.mode & S_IWOTH ? 'w' : '-');
    o << (m.mode & S_IXOTH ? 'x' : '-');
    return o;
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
  bool checkAccess(shared_ptr<Artifact> artifact, AccessFlags flags) noexcept;

  /// Get the mode field from this metadata version
  mode_t getMode() const noexcept;

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "metadata"; }

  /// Commit this version to the filesystem
  virtual void commit(fs::path path) noexcept override;

  /// Save the on-disk state to this version for later commit
  virtual void cache(fs::path path, fs::path cache_dir) noexcept override;

  /// Check if this version can be committed
  bool canCommit() const noexcept override;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    auto other_metadata = other->as<MetadataVersion>();
    if (!other_metadata) return false;
    if (other_metadata.get() == this) return true;
    return _metadata == other_metadata->_metadata;
  }

  /// Print this metadata version
  virtual ostream& print(ostream& o) const noexcept override {
    if (_metadata.has_value()) {
      return o << "[metadata: " << _metadata.value() << "]";
    } else {
      return o << "[metadata: unsaved]";
    }
  }

 private:
  optional<Metadata> _metadata;

  SERIALIZE(BASE(Version), _metadata);
};