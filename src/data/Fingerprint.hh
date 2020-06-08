#pragma once

#include <utility>

#include <sys/stat.h>

#include "data/serializer.hh"

using std::tie;

struct Fingerprint {
 public:
  bool empty;
  struct timespec mtime;

  /// Default constructor for deserialization
  Fingerprint() = default;

  /// Create a fingerprint from stat data
  Fingerprint(struct stat& s) : empty(s.st_size == 0), mtime(s.st_mtim) {}

  /// Compare to another fingerprint instance
  bool operator==(const Fingerprint& other) const {
    return tie(empty, mtime.tv_sec, mtime.tv_nsec) ==
           tie(other.empty, other.mtime.tv_sec, other.mtime.tv_nsec);
  }

  SERIALIZE(empty, mtime);
};
