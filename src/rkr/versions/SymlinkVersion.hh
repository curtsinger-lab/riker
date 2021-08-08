#pragma once

#include <filesystem>
#include <memory>
#include <sstream>
#include <string>

#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

class SymlinkVersion : public ContentVersion {
 public:
  /// Create a symlink version with a known destination
  SymlinkVersion(fs::path dest) : _dest(dest) {}

  /// Get the name for this type of version
  virtual std::string getTypeName() const noexcept override { return "symlink"; }

  /// Commit this version to the filesystem
  void commit(fs::path path) noexcept;

  /// Check if this version can be committed to the filesystem
  virtual bool canCommit() const noexcept override { return true; }

  /// Compare this version to another version
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override {
    auto other_symlink = other->as<SymlinkVersion>();
    return other_symlink && _dest == other_symlink->_dest;
  }

  /// Get the destination of this symlink
  const fs::path& getDestination() const noexcept { return _dest; }

  /// Print this symlink version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    return o << "[symlink: dest=" << _dest << "]";
  }

 private:
  fs::path _dest;
};
