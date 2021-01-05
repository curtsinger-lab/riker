#pragma once

#include <memory>
#include <ostream>
#include <string>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "data/AccessFlags.hh"
#include "ui/stats.hh"
#include "util/serializer.hh"

using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

class Artifact;
class Command;

class MetadataVersion {
 public:
  /// Create a new metadata version
  MetadataVersion(uid_t uid, gid_t gid, mode_t mode) noexcept : _uid(uid), _gid(gid), _mode(mode) {
    stats::versions++;
  }

  /// Cerate a new metadata version from a stat struct
  MetadataVersion(const struct stat& data) noexcept :
      MetadataVersion(data.st_uid, data.st_gid, data.st_mode) {}

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const noexcept { return _creator.lock(); }

  /// Record that this version was created by command c
  void createdBy(shared_ptr<Command> c) noexcept { _creator = c; }

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

  /// Compare this version to another version
  bool matches(shared_ptr<MetadataVersion> other) const noexcept;

  /// Print this metadata version
  ostream& print(ostream& o) const noexcept;

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

  /// The command that created this version
  weak_ptr<Command> _creator;

  /// The user id for this metadata version
  uid_t _uid;

  /// The group id for this metadata version
  gid_t _gid;

  /// The file mode bits for this metadata version
  mode_t _mode;

  friend class cereal::access;
  MetadataVersion() noexcept = default;
  SERIALIZE(_uid, _gid, _mode);
};