#pragma once

#include <utility>

#include <sys/stat.h>

#include "data/serializer.hh"
#include "util/log.hh"

using std::tie;

struct Metadata {
 public:
  uint16_t mode;
  uid_t uid;
  gid_t gid;
  bool empty;
  struct timespec mtime;

  /// Default constructor for deserialization
  Metadata() = default;

  /// Create a Metadata object from stat data
  Metadata(struct stat& s) :
      mode(s.st_mode), uid(s.st_uid), gid(s.st_gid), empty(s.st_size == 0), mtime(s.st_mtim) {}

  bool operator==(const Metadata& other) const {
    return tie(mode, uid, gid) == tie(other.mode, other.uid, other.gid);
  }

  SERIALIZE(mode, uid, gid, empty, mtime);
};