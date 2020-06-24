#pragma once

#include <filesystem>
#include <memory>
#include <ostream>

#include "util/serializer.hh"
#include "versions/Version.hh"

using std::ostream;
using std::shared_ptr;

namespace fs = std::filesystem;

class SymlinkVersion : public Version {
 public:
  /// Create a symlink version with a known destination
  SymlinkVersion(fs::path dest) : _dest(dest) {}

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "symlink"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const noexcept override { return true; }

  /// Save this version so it can be committed later
  void save(shared_ptr<Reference> ref) noexcept {}

  /// Commit this version to the filesystem
  void commit(shared_ptr<Reference> ref) const noexcept;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Save a fingerprint of this version
  void fingerprint(shared_ptr<Reference> ref) noexcept {}

  /// Compare this version to another version
  bool matches(shared_ptr<Version> other) const noexcept;

  /// Get the destination of this symlink
  const fs::path& getDestination() const noexcept { return _dest; }

  /// Print this symlink version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[symlink: dest=" << _dest << "]";
  }

 private:
  fs::path _dest;

  // Create a default constructor and declare fields for serialization
  SymlinkVersion() = default;
  SERIALIZE(BASE(Version), _dest);
};
