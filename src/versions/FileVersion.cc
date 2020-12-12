#include "FileVersion.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "blake3.h"
#include "interfaces/TraceHandler.hh"
#include "util/log.hh"

// Is this version saved in a way that can be committed?
bool FileVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  return _fingerprint.has_value() && _fingerprint.value().empty;
}

// Commit this version to the filesystem
void FileVersion::commit(fs::path path, mode_t mode) noexcept {
  if (isCommitted()) return;

  ASSERT(canCommit()) << "Attempted to commit unsaved version " << this << " to " << path;

  int fd = ::open(path.c_str(), O_WRONLY | O_CREAT | O_TRUNC, mode);
  FAIL_IF(fd < 0) << "Failed to commit empty file version: " << ERR;
  close(fd);

  LOG(artifact) << "Created file at " << path;

  // Mark this version as committed
  Version::setCommitted();
}

/// Save a fingerprint of this version
void FileVersion::fingerprint(TraceHandler& handler, fs::path path) noexcept {
  // if there is already a fingerprint with a hash, move on
  if (_fingerprint.has_value() && _fingerprint.value().b3hash.has_value()) return;

  // does the file actually exist?
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc != 0) return;  // leave fingerprint undefined

  // otherwise, take a fingerprint
  _fingerprint = FileFingerprint(path, statbuf, rc);
}
