#pragma once

#include <filesystem>
#include <memory>

#include "versions/Version.hh"

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
  virtual void save(shared_ptr<Reference> ref) noexcept override {}

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const noexcept override;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Save a fingerprint of this version
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept override {}

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  /// Get the destination of this symlink
  const fs::path& getDestination() const noexcept { return _dest; }

 private:
  fs::path _dest;
};
