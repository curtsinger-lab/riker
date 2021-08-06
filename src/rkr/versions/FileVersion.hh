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
#include "util/serializer.hh"
#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

class FileVersion final : public ContentVersion {
 public:
  /// The type that holds a hash of this file content version
  using Hash = std::array<uint8_t, BLAKE3_OUT_LEN>;

  /// Create a FileVersion with no existing fingerprint
  FileVersion() noexcept = default;

  /// Create a FileVersion starting with stat data for a fingerprint
  FileVersion(struct stat statbuf) noexcept :
      _empty(statbuf.st_size == 0), _mtime(statbuf.st_mtim) {}

  /// Get the name for this type of version
  virtual std::string getTypeName() const noexcept override {
    if (_empty) {
      return "file content (empty)";
    } else if (_cached) {
      return "file content (cached)";
    } else {
      return "file content";
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
  virtual bool canCoalesceWith(std::shared_ptr<ContentVersion> other) const noexcept override;

  /// Pretty printer
  virtual std::ostream& print(std::ostream& o) const noexcept override;

  /// Store a copy on disk
  void cache(fs::path path) noexcept;

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept override;

  /// Check if this file version is empty
  bool isEmpty() const noexcept { return _empty; }

  /// Check if this file version is cached
  bool isCached() const noexcept { return _cached; }

  /// Get this version's modification time
  const std::optional<struct timespec>& getModificationTime() const noexcept { return _mtime; }

  /// Get this version's hash
  const std::optional<Hash>& getHash() const noexcept { return _hash; }

 private:
  /// Compare to another fingerprint instance
  bool fingerprints_match(std::shared_ptr<FileVersion> other) const noexcept;

  /// Commit this version to the filesystem
  void commitEmptyFile(fs::path path, mode_t mode) noexcept;

  /// Restore a cached copy to the given path
  bool stage(fs::path path, mode_t mode) noexcept;

 private:
  /// Is this an empty file?
  bool _empty = false;

  /// Is there a cached copy of this file?
  bool _cached = false;

  /// When was this file version modified?
  std::optional<struct timespec> _mtime;

  /// What is the has of this file version's contents?
  std::optional<Hash> _hash;

  SERIALIZE(BASE(ContentVersion), _empty, _cached, _mtime, _hash);

  /// Transient field: has this version been linked into the new cache directory?
  bool _linked = false;
};
