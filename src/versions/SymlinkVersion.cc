#include "SymlinkVersion.hh"

#include <filesystem>
#include <memory>

using std::shared_ptr;

namespace fs = std::filesystem;

void SymlinkVersion::commit(shared_ptr<Reference> ref) const noexcept {
  // TODO: Commit symlinks
}