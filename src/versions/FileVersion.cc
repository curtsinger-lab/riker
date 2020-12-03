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

#define BLAKE3BUFSZ 65536 /* taken from BLAKE3 demo app without much thought! */

using std::nullopt;
using std::shared_ptr;

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

// Compute the blake3 hash for a path
void blake3(fs::path path, std::array<uint8_t, BLAKE3_OUT_LEN>& output) {
  // initialize hasher
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  // buffer for file read
  unsigned char buf[BLAKE3BUFSZ];

  // read from given file
  FILE* f = fopen(path.c_str(), "r");
  FAIL_IF(!f) << "Unable to fingerprint file '" << path.c_str() << "'";

  // compute hash incrementally for each chunk read
  ssize_t n;
  while ((n = fread(buf, sizeof(char), sizeof(buf), f)) > 0) {
    blake3_hasher_update(&hasher, buf, n);
  }
  fclose(f);

  // finalize the hash
  blake3_hasher_finalize(&hasher, output.data(), BLAKE3_OUT_LEN);
}

// Save a fingerprint of this version
void FileVersion::fingerprint(TraceHandler& handler, fs::path path) noexcept {
  if (hasFingerprint()) return;

  // Get stat data and save it
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);

  // Hash the file
  std::array<uint8_t, BLAKE3_OUT_LEN> hash;
  blake3(path, hash);

  // Save hash in fingerprint
  if (rc == 0) _fingerprint = FileFingerprint(statbuf, hash);
  // std::cout << "DEBUG: fingerprint of '" << path << "' is " << _fingerprint.value() << std::endl;
}
