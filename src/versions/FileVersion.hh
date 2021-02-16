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
  virtual std::string getTypeName() const noexcept override { return "file content"; }

  /// Can this version be committed to the filesystem?
  bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  virtual void commit(fs::path path) noexcept override;

  /// Save a fingerprint of this version
  virtual void fingerprint(fs::path path, FingerprintType type) noexcept override;

  /// Save an empty fingerprint of this version
  void makeEmptyFingerprint() noexcept;

  /// Compare this version to another version
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override;

  /// Pretty printer
  virtual std::ostream& print(std::ostream& o) const noexcept override;

  /// Store a copy on disk
  virtual void cache(fs::path path) noexcept override;

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept override;

 private:
  /// Compare to another fingerprint instance
  bool fingerprints_match(std::shared_ptr<FileVersion> other) const noexcept;

  /// Commit this version to the filesystem
  void commitEmptyFile(fs::path path, mode_t mode = 0600) noexcept;

  /// Restore a cached copy to the given path
  bool stage(fs::path path) noexcept;

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
