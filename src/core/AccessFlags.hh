#pragma once

#include <ostream>
#include <utility>

#include <fcntl.h>

#include "tracing/Flags.hh"
#include "util/serializer.hh"

using std::ostream;
using std::pair;

/// This struct encodes the flags specified when making an access to a particular reference
struct AccessFlags {
  union {
    struct {
      bool r : 1;          //< Does the reference require read access?
      bool w : 1;          //< Does the reference require write access?
      bool x : 1;          //< Does the reference require execute access?
      bool nofollow : 1;   //< Does the reference resolve to a symlink?
      bool truncate : 1;   //< Does the reference truncate the artifact's contents?
      bool create : 1;     //< Does the reference create an artifact if none exists?
      bool exclusive : 1;  //< Does the reference require creation?
      bool append : 1;     //< Is the file opened in append mode?
      bool directory : 1;  //< Is the open expected to return a directory?
    };
    uint16_t _data = 0;
  };
  uint16_t mode = 0;  //< The file access modifiers

  // Declare fields for serialization
  template <class Archive>
  void serialize(Archive& archive) {
    // Serialze the integer representation of all the relevant flags
    archive(_data);
    // Only include the mode field if the create flag is set
    if (create) archive(mode);
  }

  /// Create an AccessFlags instance from the flags parameter to the open syscall
  static AccessFlags fromOpen(o_flags flags, mode_flags mode) noexcept {
    AccessFlags f;
    f.r = flags.readable();
    f.w = flags.writable();
    f.nofollow = flags.nofollow();
    f.truncate = flags.trunc();
    f.create = flags.creat();
    f.exclusive = flags.excl();
    f.append = flags.append();
    f.directory = flags.directory();
    f.mode = mode.getMode();
    return f;
  }

  /// Generate flags for the open() call from this AccessFlags instance
  pair<int, uint16_t> toOpen() const noexcept {
    int flags = 0;
    if (r && w) flags |= O_RDWR;
    if (r && !w) flags |= O_RDONLY;
    if (!r && w) flags |= O_WRONLY;
    if (nofollow) flags |= O_NOFOLLOW;
    if (truncate) flags |= O_TRUNC;
    if (create) flags |= O_CREAT;
    if (exclusive) flags |= O_EXCL;
    if (append) flags |= O_APPEND;
    if (directory) flags |= O_DIRECTORY;

    return {flags, mode};
  }

  /// Create an AccessFlags instance from the mode and flags parameters to the access syscall
  static AccessFlags fromAccess(int mode, int flags) noexcept {
    AccessFlags f;
    f.r = (mode & R_OK) == R_OK;
    f.w = (mode & W_OK) == W_OK;
    f.x = (mode & X_OK) == X_OK;
    f.nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
    return f;
  }

  /// Generate mode and flags for the access() call from this AccessFlags instance
  pair<int, int> toAccess() const noexcept {
    int mode = 0;
    if (r) mode |= R_OK;
    if (w) mode |= W_OK;
    if (x) mode |= X_OK;

    int flags = AT_EACCESS;
    if (nofollow) flags |= AT_SYMLINK_NOFOLLOW;

    return {mode, flags};
  }

  /// Create an AccessFlags instance from the flags parameter to the stat syscall
  static AccessFlags fromStat(int flags) noexcept {
    AccessFlags f;
    f.nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW;
    return f;
  }

  /// Generate flags for the fstatat() call from this AccessFlags instance
  int toStat() const noexcept { return nofollow ? AT_SYMLINK_NOFOLLOW : 0; }

  /// Print an AccessFlags struct to an output stream
  friend ostream& operator<<(ostream& o, const AccessFlags& f) noexcept {
    o << (f.r ? 'r' : '-') << (f.w ? 'w' : '-') << (f.x ? 'x' : '-')
      << (f.nofollow ? " nofollow" : "") << (f.truncate ? " truncate" : "")
      << (f.create ? " create" : "") << (f.exclusive ? " exclusive" : "")
      << (f.append ? " append" : "");

    if (f.mode != 0) {
      o << " (";
      o << (f.mode & S_IRUSR ? 'r' : '-');
      o << (f.mode & S_IWUSR ? 'w' : '-');
      o << (f.mode & S_IXUSR ? 'x' : '-');
      o << (f.mode & S_IRGRP ? 'r' : '-');
      o << (f.mode & S_IWGRP ? 'w' : '-');
      o << (f.mode & S_IXGRP ? 'x' : '-');
      o << (f.mode & S_IROTH ? 'r' : '-');
      o << (f.mode & S_IWOTH ? 'w' : '-');
      o << (f.mode & S_IXOTH ? 'x' : '-');
      o << ")";
    }

    return o;
  }
};
