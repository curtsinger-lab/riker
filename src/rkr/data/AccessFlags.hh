#pragma once

#include <ostream>
#include <utility>

#include <fcntl.h>

#include "tracing/Flags.hh"
#include "util/log.hh"

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int8_t { SUCCESS = 0 };

enum class ArtifactType : uint8_t { File, Symlink, Dir };

/// An AccessType encodes the error code (or success) outcomes for a reference depending on the typ
/// of artifact that is reached. Any time an AccessType encodes an error code other than SUCCESS
/// that will be the result of the access if it reaches the corresponding artifact type as the
/// _final_ entry along the path.
class AccessType {
 public:
  constexpr AccessType() noexcept = default;

  constexpr AccessType(int8_t rc_file, int8_t rc_symlink, int8_t rc_dir) noexcept :
      _rc_file(rc_file), _rc_symlink(rc_symlink), _rc_dir(rc_dir) {}

  constexpr static AccessType any() noexcept { return AccessType(SUCCESS, SUCCESS, SUCCESS); }

  constexpr static AccessType file() noexcept { return AccessType(SUCCESS, ELOOP, EISDIR); }

  constexpr static AccessType symlink() noexcept { return AccessType(EINVAL, SUCCESS, EINVAL); }

  constexpr static AccessType dir() noexcept { return AccessType(ENOTDIR, ENOTDIR, SUCCESS); }

  constexpr static AccessType notSymlink() noexcept { return AccessType(SUCCESS, ELOOP, SUCCESS); }

  constexpr static AccessType notDir() noexcept { return AccessType(SUCCESS, SUCCESS, EISDIR); }

  constexpr int8_t getResult(ArtifactType t) const noexcept {
    if (t == ArtifactType::File) {
      return _rc_file;
    } else if (t == ArtifactType::Symlink) {
      return _rc_symlink;
    } else if (t == ArtifactType::Dir) {
      return _rc_dir;
    } else {
      WARN << "Unknown artifact type";
      return EBADF;
    }
  }

  AccessType intersect(const AccessType& other) const noexcept {
    ASSERT(_rc_file == SUCCESS || other._rc_file == SUCCESS || _rc_file == other._rc_file)
        << "Conflicting outcomes for file artifact";

    ASSERT(_rc_symlink == SUCCESS || other._rc_symlink == SUCCESS ||
           _rc_symlink == other._rc_symlink)
        << "Conflicting outcomes for symlink artifact";

    ASSERT(_rc_dir == SUCCESS || other._rc_dir == SUCCESS || _rc_dir == other._rc_dir)
        << "Conflicting outcomes for dir artifact";

    return AccessType(_rc_file == SUCCESS ? other._rc_file : _rc_file,
                      _rc_symlink == SUCCESS ? other._rc_symlink : _rc_symlink,
                      _rc_dir == SUCCESS ? other._rc_dir : _rc_dir);
  }

 private:
  int8_t _rc_file = SUCCESS;
  int8_t _rc_symlink = SUCCESS;
  int8_t _rc_dir = SUCCESS;
};

/// This struct encodes the flags specified when making an access to a particular reference
class AccessFlags {
 public:
  constexpr AccessFlags() noexcept = default;

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
      bool directory : 1;  //< Is the opened inode required to be a directory?
    };
    uint16_t _data = 0;
  };
  AccessType type;    //< What type of artifact is this access expected to reach?
  uint16_t mode = 0;  //< The file access modifiers

  // Declare fields for serialization
  template <class Archive>
  void serialize(Archive& archive) {
    // Serialze the integer representation of all the relevant flags
    archive(_data, type);
    // Only include the mode field if the create flag is set
    if (create) archive(mode);
  }

  /// Create an AccessFlags instance from the flags parameter to the open syscall
  static AccessFlags fromOpen(o_flags flags, mode_flags mode, mode_t umask) noexcept {
    AccessFlags f;
    f.r = flags.readable();
    f.w = flags.writable();
    f.nofollow = flags.nofollow();
    f.truncate = flags.trunc();
    f.create = flags.creat();
    f.exclusive = flags.excl();
    f.append = flags.append();
    f.directory = flags.directory();

    // NOTE:
    // O_DIRECTORY does not imply AccessType::Dir. See `tests/openat-directory/01-build.t`.

    // these flags specifically state that they only apply to regular files
    if (flags.creat() || flags.append() || (flags.writable() && flags.trunc())) {
      f.type = AccessType::file();
    }

    // If nofollow is specified the final entity cannot be a symlink
    if (flags.nofollow() && !flags.directory()) {
      f.type = AccessType::notSymlink();
    }

    f.mode = mode.getMode() & ~umask;
    return f;
  }

  /// Generate flags for the open() call from this AccessFlags instance
  std::pair<int, uint16_t> toOpen() const noexcept {
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
  static AccessFlags fromAccess(int mode, at_flags flags) noexcept {
    AccessFlags f;
    f.r = (mode & R_OK) == R_OK;
    f.w = (mode & W_OK) == W_OK;
    f.x = (mode & X_OK) == X_OK;
    f.nofollow = flags.symlink_nofollow();
    return f;
  }

  /// Generate mode and flags for the access() call from this AccessFlags instance
  std::pair<int, int> toAccess() const noexcept {
    int mode = 0;
    if (r) mode |= R_OK;
    if (w) mode |= W_OK;
    if (x) mode |= X_OK;

    int flags = AT_EACCESS;
    if (nofollow) flags |= AT_SYMLINK_NOFOLLOW;

    return {mode, flags};
  }

  /// Create an AccessFlags instance from the flags parameter to the stat syscall
  static AccessFlags fromAtFlags(at_flags flags) noexcept {
    AccessFlags f;
    f.nofollow = flags.symlink_nofollow();
    return f;
  }

  /// Generate flags for the fstatat() call from this AccessFlags instance
  int toStat() const noexcept { return nofollow ? AT_SYMLINK_NOFOLLOW : 0; }

  /// Print an AccessFlags struct to an output stream
  friend std::ostream& operator<<(std::ostream& o, const AccessFlags& f) noexcept {
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

constexpr inline static AccessFlags NoAccess{};
constexpr inline static AccessFlags ReadAccess{.r = true};
constexpr inline static AccessFlags WriteAccess{.w = true};
constexpr inline static AccessFlags ExecAccess{.x = true};
constexpr inline static AccessFlags NoFollowAccess{.nofollow = true};

constexpr inline static AccessFlags DirAccess{.type = AccessType::dir()};
constexpr inline static AccessFlags NotDirAccess{.type = AccessType::notDir()};
constexpr inline static AccessFlags SymlinkAccess{.type = AccessType::symlink()};

constexpr inline static AccessFlags operator+(const AccessFlags& f1,
                                              const AccessFlags& f2) noexcept {
  AccessFlags result;
  result._data = f1._data | f2._data;
  result.type = f1.type.intersect(f2.type);
  return result;
}
