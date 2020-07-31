#include "SymlinkVersion.hh"

#include <filesystem>
#include <memory>

#include <fcntl.h>
#include <unistd.h>

#include "core/IR.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

void SymlinkVersion::commit(fs::path path) noexcept {
  if (isCommitted()) return;

  int rc = ::symlink(_dest.c_str(), path.c_str());
  FAIL_IF(rc != 0) << "Failed to commit " << this << " to " << path;

  // Mark this version as committed
  Version::setCommitted();
}