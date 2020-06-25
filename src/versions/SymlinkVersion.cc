#include "SymlinkVersion.hh"

#include <filesystem>
#include <memory>

#include <fcntl.h>
#include <unistd.h>

#include "core/IR.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

void SymlinkVersion::commit(shared_ptr<Reference> ref) noexcept {
  if (isCommitted()) return;

  auto access = ref->as<Access>();
  ASSERT(access) << "Tried to commit a symlink to a non-path reference";

  int rc = ::symlink(_dest.c_str(), access->getFullPath().c_str());
  FAIL_IF(rc != 0) << "Failed to commit " << this << " to " << access;

  // Mark this version as committed
  Version::setCommitted();
}