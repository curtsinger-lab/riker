#pragma once

#include <array>
#include <cstdint>
#include <ctime>
#include <filesystem>
#include <memory>
#include <optional>
#include <sstream>
#include <string>

#include <sys/stat.h>

#include "blake3.h"
#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

class SocketVersion final : public ContentVersion {
 public:
  /// The type that holds a hash of this socket content version
  using Hash = std::array<uint8_t, BLAKE3_OUT_LEN>;

  /// Create a SocketVersion with no existing fingerprint
  SocketVersion() noexcept = default;

  /// Create a SocketVersion starting with stat data for a fingerprint
  SocketVersion(struct stat statbuf) noexcept :
      _empty(statbuf.st_size == 0), _mtime(statbuf.st_mtim) {}

  /// Create a socket version from serialized data
  SocketVersion(bool empty,
                bool cached,
                std::optional<struct timespec> mtime,
                std::optional<Hash> hash) noexcept :
      _empty(empty), _cached(cached), _mtime(mtime), _hash(hash) {}

  /// Get the name for this type of version
  virtual std::string getTypeName() const noexcept override {
    if (_empty) {
      return "socket content (empty)";
    } else if (_cached) {
      return "socket content (cached)";
    } else {
      return "socket content";
    }
  }

  /// Can this version be committed to the filesystem?
  bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  void commit(fs::path path, mode_t mode = 0) noexcept;

  /// Save a fingerprint of this version
  void fingerprint(fs::path path, FingerprintType type) noexcept;

  /// Save an empty fingerprint of this version
  void makeEmptyFingerprint() noexcept;

  /// Compare this version to another version
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override;

  /// Can a write of this version be coalesced with another?
  // virtual bool canCoalesceWith(std::shared_ptr<ContentVersion> other) const noexcept override;

  /// Pretty printer
  virtual std::ostream& print(std::ostream& o) const noexcept override;

  /// Store a copy on disk
  void cache(fs::path path) noexcept;

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept override;

  /// Check if this socket version is empty
  bool isEmpty() const noexcept { return _empty; }

  /// Check if this socket version is cached
  bool isCached() const noexcept { return _cached; }

  /// Get this version's modification time
  const std::optional<struct timespec>& getModificationTime() const noexcept { return _mtime; }

  /// Get this version's hash
  const std::optional<Hash>& getHash() const noexcept { return _hash; }

 private:
  /// Compare to another fingerprint instance
  bool fingerprints_match(std::shared_ptr<SocketVersion> other) const noexcept;

  /// Commit this version to the filesystem
  void commitEmptySocket(fs::path path, mode_t mode) noexcept;

  /// Restore a cached copy to the given path
  bool stage(fs::path path, mode_t mode) noexcept;

 private:
  /// Is this an empty socket?
  bool _empty = false;

  /// Is there a cached copy of this socket?
  bool _cached = false;

  /// When was this socket version modified?
  std::optional<struct timespec> _mtime;

  /// What is the has of this socket version's contents?
  std::optional<Hash> _hash;

  /// Transient field: has this version been linked into the new cache directory?
  bool _linked = false;
};
