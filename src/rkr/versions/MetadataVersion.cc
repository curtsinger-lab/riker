#include "MetadataVersion.hh"

#include <cstdint>
#include <filesystem>
#include <map>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "data/AccessFlags.hh"
#include "util/log.hh"
#include "util/wrappers.hh"

using std::make_shared;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

/// Create a new metadata version by changing the owner and/or group in this one
shared_ptr<MetadataVersion> MetadataVersion::chown(uid_t user, gid_t group) const noexcept {
  return make_shared<MetadataVersion>(user, group, _mode);
}

/// Create a new metadata version by changing the mode bits in this one
shared_ptr<MetadataVersion> MetadataVersion::chmod(mode_t mode) const noexcept {
  return make_shared<MetadataVersion>(_uid, _gid, (_mode & S_IFMT) | mode);
}

bool MetadataVersion::checkAccess(AccessFlags flags) noexcept {
  // TODO: Currently checking against the rkr process' effective user and group(s). This check
  // should instead use the user and group(s) of the emulated process/command. Such attributes
  // should probably be stored within the reference.

  // if the user's effective uid is root, bypass all checks
  auto [gid, uid] = get_identity();
  if (uid == 0) return true;

  // Keep track of which permissions we still need to satisfy
  bool read_needed = flags.r;
  bool write_needed = flags.w;
  bool execute_needed = flags.x;

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to the owner
  if (uid == _uid) {
    if (_mode & S_IRUSR) read_needed = false;
    if (_mode & S_IWUSR) write_needed = false;
    if (_mode & S_IXUSR) execute_needed = false;
  }

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to the primary group
  if (gid == _gid) {
    if (_mode & S_IRGRP) read_needed = false;
    if (_mode & S_IWGRP) write_needed = false;
    if (_mode & S_IXGRP) execute_needed = false;
  }

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to world
  if (_mode & S_IROTH) read_needed = false;
  if (_mode & S_IWOTH) write_needed = false;
  if (_mode & S_IXOTH) execute_needed = false;

  // If we've satisfied all needed permissions, stop checking
  if (!read_needed && !write_needed && !execute_needed) return true;

  // Check for permissions granted to the supplementary groups
  if (getgroups().find(_gid) != getgroups().end()) {
    if (_mode & S_IRGRP) read_needed = false;
    if (_mode & S_IWGRP) write_needed = false;
    if (_mode & S_IXGRP) execute_needed = false;
  }

  // Access is granted as long as all requested permissions were granted
  return !read_needed && !write_needed && !execute_needed;
}

// Commit this version to the filesystem
void MetadataVersion::commit(fs::path path) noexcept {
  int rc = ::lchown(path.c_str(), _uid, _gid);
  FAIL_IF(rc != 0) << "Failed to commit owner and group to " << path << ": " << ERR;

  rc = ::chmod(path.c_str(), _mode);
  FAIL_IF(rc != 0) << "Failed to commit permissions to " << path << ": " << ERR;
}

// Compare two metadata versions
bool MetadataVersion::matches(shared_ptr<MetadataVersion> other) const noexcept {
  // Compare uids
  if (_uid != other->_uid) return false;

  // Compare gids
  if (_gid != other->_gid) return false;

  // Compare types
  if ((_mode & S_IFMT) != (other->_mode & S_IFMT)) return false;

  // Note: We are not comparing permission bits. This is intentional. We only care about permission
  // changes that influence path resolution, which is modeled separately

  return true;
}

static map<uint16_t, string> modes = {
    {S_IFSOCK, "sock"}, {S_IFLNK, "symlink"}, {S_IFREG, "file"}, {S_IFBLK, "blockdev"},
    {S_IFDIR, "dir"},   {S_IFCHR, "chardev"}, {S_IFIFO, "fifo"}};

// Print a metadata version
ostream& MetadataVersion::print(ostream& o) const noexcept {
  o << "[metadata: ";
  o << "uid=" << _uid << ", ";
  o << "gid=" << _gid << ", ";
  o << "type=" << modes[_mode & S_IFMT] << ", ";
  o << "perms=";
  o << (_mode & S_IRUSR ? 'r' : '-');
  o << (_mode & S_IWUSR ? 'w' : '-');
  o << (_mode & S_IXUSR ? 'x' : '-');
  o << (_mode & S_IRGRP ? 'r' : '-');
  o << (_mode & S_IWGRP ? 'w' : '-');
  o << (_mode & S_IXGRP ? 'x' : '-');
  o << (_mode & S_IROTH ? 'r' : '-');
  o << (_mode & S_IWOTH ? 'w' : '-');
  o << (_mode & S_IXOTH ? 'x' : '-');
  o << "]";
  return o;
}