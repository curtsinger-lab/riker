#pragma once

#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "blake3.h"
#include "string.h"
#include "ui/options.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::stringstream;

#define BLAKE3BUFSZ 65536 /* taken from BLAKE3 demo app without much thought! */

typedef std::array<uint8_t, BLAKE3_OUT_LEN> BLAKE3Hash;

class Ref;

class FileVersion final : public Version {
 public:
  /// Create a FileVersion with no existing fingerprint
  FileVersion() noexcept = default;

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "content"; }

  /// Can this version be committed to the filesystem?
  bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  void commitWithMode(fs::path path, mode_t mode = 0600) noexcept;

  /// Save a fingerprint of this version
  virtual void fingerprint(fs::path path, fs::path cache_dir) noexcept override;

  /// Does this FileVersion have a fingerprint already?
  bool hasHash() noexcept;

  /// Save an empty fingerprint of this version
  void makeEmptyFingerprint() noexcept;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  /// get a string representation of the hash
  string b3hex() const noexcept;

  /// Pretty printer
  virtual ostream& print(ostream& o) const noexcept override;

  /// Return the path for the contents of this cached FileVersion relative to the given cache_dir
  fs::path cacheFilePath(fs::path cache_dir) noexcept;

 private:
  bool _empty;
  std::optional<struct timespec> _mtime;
  std::optional<BLAKE3Hash> _b3hash;

  /// Compare to another fingerprint instance
  bool fingerprints_match(shared_ptr<FileVersion> other) const noexcept;

  /// Convert a BLAKE3 byte array to a hexadecimal string
  static string b3hex(BLAKE3Hash b3hash) noexcept;

  /// Return a BLAKE3 hash for the contents of the file at the given path.
  static std::optional<BLAKE3Hash> blake3(fs::path path) noexcept;

  /// Return the path for the contents of this cached FileVersion
  static fs::path cacheFilePath(BLAKE3Hash& hash, fs::path cache_dir) noexcept;

  void cache(const struct stat& statbuf,
             BLAKE3Hash& hash,
             fs::path path,
             fs::path cache_dir) noexcept;

  SERIALIZE(BASE(Version), _empty, _mtime, _b3hash);
};
