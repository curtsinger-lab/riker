#include "SymlinkVersion.hh"

#include <cerrno>
#include <cstring>
#include <filesystem>
#include <memory>

#include <sys/stat.h>
#include <unistd.h>

#include "tracing/Flags.hh"
#include "util/log.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

void SymlinkVersion::commit(fs::path path) noexcept {
  if (isCommitted()) return;

  struct stat statbuf;

  // does the file already exist?
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc == -1) {
    if (errno == ENOENT) {
      // file doesn't exist-- go ahead and create a symlink
      rc = ::symlink(_dest.c_str(), path.c_str());

      FAIL_IF(rc != 0) << "Failed to commit " << this << " to " << path << " (" << strerror(errno)
                       << ")";
    } else {
      // some error we're not expecting
      FAIL_IF(rc != 0) << "Unable to stat " << path << " (" << strerror(errno) << ")";
    }
  } else {
    // decode
    mode_flags flags(statbuf.st_mode);

    // file exists-- but is it a symlink?
    FAIL_IF(!flags.isSymlink()) << "File at " << path
                                << " exists but is not a symlink as expected.  File is "
                                << flags.filetype_str() << " (" << (statbuf.st_mode & S_IFMT)
                                << ").";
  }

  // Mark this version as committed
  ContentVersion::setCommitted();
}