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
  return _empty;
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
void FileVersion::fingerprint(fs::path path, fs::path cache_dir) noexcept {
  // if there is already a fingerprint with a hash, move on
  if (hasHash()) return;

  // does the file actually exist?
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc != 0) return;  // leave mtime and hash undefined

  // otherwise, take a "fingerprint"
  // save mtime from statbuf
  _empty = statbuf.st_size == 0;
  _mtime = statbuf.st_mtim;

  // if file is readable and hashable, save hash
  if (rc == 0) {
    // is this a regular file?
    if (!(statbuf.st_mode & S_IFREG)) return;
    _b3hash = blake3(path);

    // if caching is enabled, cache the file
    if (options::enable_cache && _b3hash.has_value()) {
      cache(statbuf, _b3hash.value(), path, cache_dir);
    }
  }
}

/// Does this FileVersion have a hash already?
bool FileVersion::hasHash() noexcept {
  return _b3hash.has_value();
}

void FileVersion::makeEmptyFingerprint() noexcept {
  // it is not necessary to fingerprint or cache empty files
  _empty = true;
}

void FileVersion::cache(const struct stat& statbuf,
                        BLAKE3Hash& hash,
                        fs::path path,
                        fs::path cache_dir) noexcept {
  // Path to cache file
  fs::path hash_file = cacheFilePath(hash, cache_dir);
  fs::path hash_dir = hash_file.parent_path();

  // Get the length of the input file
  loff_t len = statbuf.st_size;

  // Does the cache file already exist?  If so, skip. (reuse statbuf)
  struct stat cache_statbuf;
  bool file_exists(::lstat(hash_file.c_str(), &cache_statbuf) == 0);
  if (file_exists) return;

  // Create the directories, if needed
  fs::create_directories(hash_dir);

  // Open source and destination fds
  int src_fd = ::open(path.c_str(), O_RDONLY);
  FAIL_IF(src_fd == -1) << "Unable to open file '" << path << "' for caching: " << ERR;
  int dst_fd = ::open(hash_file.c_str(), O_CREAT | O_EXCL | O_WRONLY);
  FAIL_IF(dst_fd == -1) << "Unable to create cache file '" << hash_file << "' for file '" << path
                        << "': " << ERR;

  // copy file to cache using non-POSIX fast copy
  loff_t bytes_cp;
  do {
    bytes_cp = ::copy_file_range(src_fd, NULL, dst_fd, NULL, len, 0);
    FAIL_IF(bytes_cp == -1) << "Could not copy file '" << path << "' to cache location '"
                            << hash_file << "': " << ERR;

    len -= bytes_cp;
  } while (len > 0 && bytes_cp > 0);

  LOG(artifact) << "Cached file version at path '" << path << "' in '" << hash_file << "'";

  close(src_fd);
  close(dst_fd);
}
