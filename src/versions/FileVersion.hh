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
  bool canCommit() const noexcept;

  /// Commit this version to the filesystem
  void commit(fs::path path, mode_t mode = 0600) noexcept;

  /// Save a fingerprint of this version
  virtual void fingerprint(fs::path path, fs::path cache_dir) noexcept override;

  /// Does this FileVersion have a fingerprint already?
  bool hasHash() noexcept;

  /// Save an empty fingerprint of this version
  void makeEmptyFingerprint() noexcept;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    auto other_file = other->as<FileVersion>();
    if (!other_file) return false;
    if (other_file.get() == this) return true;
    return fingerprints_match(other_file);
  }

  /// Compare to another fingerprint instance
  bool fingerprints_match(shared_ptr<FileVersion> other) const noexcept {
    // Two empty files are always equivalent
    if (_empty && other->_empty) {
      LOG(artifact) << "Not checking equality for fingerprint: both files are empty.";
      return true;
    }

    // Do the mtimes match?
    if (_mtime.has_value() && other->_mtime.has_value()) {
      auto m1 = _mtime.value();
      auto m2 = other->_mtime.value();
      if (m1.tv_sec == m2.tv_sec && m1.tv_nsec == m2.tv_nsec) {
        // Yes. Return a match immediately
        LOG(artifact) << "mtimes match.";
        return true;
      }
    }

    // If fingerprinting is enabled, check to see if we have a hash and the hashes match
    if (!options::mtime_only && _b3hash.has_value() && other->_b3hash.has_value() &&
        _b3hash.value() == other->_b3hash.value()) {
      LOG(artifact) << "Fingerprints match";
      return true;
    }

    // If fingerprinting is disabled but the hashes match, print some info
    if (options::mtime_only && _b3hash.has_value() && other->_b3hash.has_value() &&
        _b3hash.value() == other->_b3hash.value()) {
      LOG(artifact) << "Fingerprints match, but mtimes do not";
    }

    return false;
  }

  // get a string representation of the hash
  string b3hex() const noexcept {
    if (!_b3hash.has_value()) {
      return "NO HASH";
    }
    return b3hex(_b3hash.value());
  }

  virtual ostream& print(ostream& o) const noexcept override {
    // is empty
    if (_empty) return o << "[file content: empty]";

    // not empty, no mtime, no hash
    if (!_mtime.has_value() && !_b3hash.has_value()) return o << "[file content: unknown]";

    // has mtime
    o << "[file content: ";
    if (_mtime.has_value())
      o << "mtime=" << _mtime.value().tv_sec << "." << std::setfill('0') << std::setw(9)
        << _mtime.value().tv_nsec << " ";

    // has hash
    if (_b3hash.has_value()) o << "b3hash=" << b3hex();

    o << "]";

    return o;
  }

  /// Return the path for the contents of this cached FileVersion relative to the given cache_dir
  fs::path cacheFilePath(fs::path cache_dir) {
    ASSERT(_b3hash.has_value()) << "Cannot obtain cache location for unfingerprinted file.";
    return cacheFilePath(_b3hash.value(), cache_dir);
  }

 private:
  bool _empty;
  std::optional<struct timespec> _mtime;
  std::optional<BLAKE3Hash> _b3hash;

  static string b3hex(BLAKE3Hash b3hash) noexcept {
    stringstream ss;
    for (int byte : b3hash) {
      ss << std::setfill('0') << std::setw(2) << std::hex << byte;
    }
    return ss.str();
  }

  /// Return a BLAKE3 hash for the contents of the file at the given path.
  static std::optional<BLAKE3Hash> blake3(fs::path path) {
    // initialize hasher
    blake3_hasher hasher;
    blake3_hasher_init(&hasher);

    // buffer for file read
    unsigned char buf[BLAKE3BUFSZ];

    // read from given file
    LOG(exec) << "Fingerprinting " << path;
    FILE* f = fopen(path.c_str(), "r");
    if (!f) {
      LOG(artifact) << "Unable to fingerprint file '" << path.c_str() << "': " << ERR;
      return nullopt;
    }

    // create output array
    BLAKE3Hash output;

    // compute hash incrementally for each chunk read
    ssize_t n;
    while ((n = fread(buf, sizeof(char), sizeof(buf), f)) > 0) {
      blake3_hasher_update(&hasher, buf, n);
    }
    fclose(f);

    // finalize the hash
    blake3_hasher_finalize(&hasher, output.data(), BLAKE3_OUT_LEN);

    return output;
  }

  /// Return the path for the contents of this cached FileVersion
  static fs::path cacheFilePath(BLAKE3Hash& hash, fs::path cache_dir) {
    // We use a three-level directory prefix scheme to store cached files
    // to avoid having too many files in a given folder.  This scheme
    // below has 16^6 unique directory prefixes.
    string hash_str = b3hex(hash);
    fs::path dir_lvl_0 = hash_str.substr(0, 2);
    fs::path dir_lvl_1 = hash_str.substr(2, 2);
    fs::path dir_lvl_2 = hash_str.substr(4, 2);
    fs::path hash_dir = cache_dir / dir_lvl_0 / dir_lvl_1 / dir_lvl_2;

    // Path to cache file
    return hash_dir / hash_str;
  }

  void cache(const struct stat& statbuf,
             BLAKE3Hash& hash,
             fs::path path,
             fs::path cache_dir) noexcept;

  SERIALIZE(BASE(Version), _empty, _mtime, _b3hash);
};
