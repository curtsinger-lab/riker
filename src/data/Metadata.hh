#pragma once

#include <utility>

#include <sys/stat.h>

#include "data/serializer.hh"

using std::tie;

struct Metadata {
 public:
  uint16_t mode;
  uid_t uid;
  gid_t gid;

  /// Default constructor for deserialization
  Metadata() = default;

  /// Create a Metadata object from stat data
  Metadata(struct stat& s) : mode(s.st_mode), uid(s.st_uid), gid(s.st_gid) {}

  /// Compare to another Metadata instance
  bool operator==(const Metadata& other) const {
    return tie(mode, uid, gid) == tie(other.mode, other.uid, other.gid);
  }

  SERIALIZE(mode, uid, gid);
};