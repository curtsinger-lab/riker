#include "SymlinkVersion.hh"

#include <filesystem>
#include <memory>

using std::shared_ptr;

namespace fs = std::filesystem;

void SymlinkVersion::commit(shared_ptr<Reference> ref) const noexcept {
  // TODO: Commit symlinks
}

bool SymlinkVersion::matches(shared_ptr<Version> other) const noexcept {
  // Make sure the other version is a symlink
  auto other_symlink = other->as<SymlinkVersion>();
  if (!other_symlink) return false;

  // Compare the destinations
  return _dest == other_symlink->_dest;
}