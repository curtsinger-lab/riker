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
#include "versions/ContentVersion.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::stringstream;

#define BLAKE3BUFSZ 65536 /* taken from BLAKE3 demo app without much thought! */

typedef std::array<uint8_t, BLAKE3_OUT_LEN> BLAKE3Hash;

class Ref;

class FileVersion final : public ContentVersion {
 public:
  /// Create a FileVersion with no existing fingerprint
  FileVersion() noexcept = default;

  /// Create a FileVersion starting with stat data for a fingerprint
  FileVersion(struct stat statbuf) noexcept :
      _empty(statbuf.st_size == 0), _mtime(statbuf.st_mtim) {}

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "content"; }

  /// Can this version be committed to the filesystem?
  bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  virtual void commit(fs::path path) noexcept override;

  /// Save a fingerprint of this version
  virtual void fingerprint(fs::path path) noexcept override;

  /// Save an empty fingerprint of this version
  void makeEmptyFingerprint() noexcept;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<ContentVersion> other) const noexcept override;

  /// Pretty printer
  virtual ostream& print(ostream& o) const noexcept override;

  /// Store a copy on disk
  virtual void cache(fs::path path) noexcept override;

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept override;

 private:
  /// Compare to another fingerprint instance
  bool fingerprints_match(shared_ptr<FileVersion> other) const noexcept;

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
  std::optional<BLAKE3Hash> _b3hash;

  SERIALIZE(BASE(ContentVersion), _empty, _cached, _mtime, _b3hash);

  /// Transient field: has this version been linked into the new cache directory?
  bool _linked = false;
};
