#pragma once

#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "util/serializer.hh"
#include "versions/Version.hh"

using std::map;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;

class Reference;

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
  bool checkAccess(AccessFlags flags) noexcept;

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "metadata"; }

  /// Is this version saved in a way that can be committed?
  bool isSaved() const noexcept { return _metadata.has_value(); }

  /// Save this version so it can be committed later
  void save(shared_ptr<Reference> ref) noexcept;

  /// Commit this version to the filesystem
  void commit(shared_ptr<Reference> ref) const noexcept;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  bool hasFingerprint() const noexcept { return _metadata.has_value(); }

  /// Save a fingerprint of this version
  void fingerprint(shared_ptr<Reference> ref) noexcept { save(ref); }

  /// Compare this version to another version
  bool matches(shared_ptr<MetadataVersion> other) const noexcept {
    if (other.get() == this) return true;
    return _metadata == other->_metadata;
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