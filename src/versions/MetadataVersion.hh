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
#include "ui/stats.hh"
#include "util/serializer.hh"

using std::map;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

class Artifact;
class CommandRun;

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

class MetadataVersion {
 public:
  /// Create a new metadata version with unknown metadata
  MetadataVersion() noexcept { stats::versions++; }

  /// Cerate a new metadata version with existing metadata
  MetadataVersion(Metadata&& m) noexcept : _metadata(m) {}

  /// Get the command that created this version
  shared_ptr<CommandRun> getCreator() const noexcept { return _creator.lock(); }

  /// Record that this version was created by command c
  void createdBy(shared_ptr<CommandRun> r) noexcept { _creator = r; }

  /// Check if this version has been committed
  bool isCommitted() const noexcept { return _committed; }

  /// Mark this version as committed
  void setCommitted(bool committed = true) noexcept { _committed = committed; }

  /// Check if a given access is allowed by the mode bits in this metadata record
  bool checkAccess(shared_ptr<Artifact> artifact, AccessFlags flags) noexcept;

  /// Get the mode field from this metadata version
  mode_t getMode() const noexcept;

  /// Commit this version to the filesystem
  void commit(fs::path path) noexcept;

  /// Save the on-disk state to this version for later commit
  void cache(fs::path path) noexcept;

  /// Check if this version can be committed
  bool canCommit() const noexcept;

  /// Compare this version to another version
  bool matches(shared_ptr<MetadataVersion> other) const {
    if (!other) return false;
    if (other.get() == this) return true;
    return _metadata == other->_metadata;
  }

  /// Print this metadata version
  ostream& print(ostream& o) const noexcept {
    if (_metadata.has_value()) {
      return o << "[metadata: " << _metadata.value() << "]";
    } else {
      return o << "[metadata: unsaved]";
    }
  }

  /// Print a Version
  friend ostream& operator<<(ostream& o, const MetadataVersion& v) noexcept { return v.print(o); }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const MetadataVersion* v) noexcept {
    if (v == nullptr) return o << "<null MetadataVersion>";
    return v->print(o);
  }

 private:
  /// Has this version been committed?
  bool _committed = false;

  /// The command run that created this version
  weak_ptr<CommandRun> _creator;

  optional<Metadata> _metadata;

  SERIALIZE(_metadata);
};