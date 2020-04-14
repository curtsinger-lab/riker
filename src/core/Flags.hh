#pragma once

/// This struct encodes the flags specified when making an access to a particular reference
struct Flags {
  bool r = false;          // Does the reference require read access?
  bool w = false;          // Does the reference require write access?
  bool x = false;          // Does the reference require execute access?
  bool nofollow = false;   // Does the reference resolve to a symlink rather than its target?
  bool truncate = false;   // Does the reference truncate the artifact's contents?
  bool create = false;     // Does the reference create an artifact if none exists?
  bool exclusive = false;  // Does the reference require creation? (must also be set with .create

  /// Create a Flags instance from the flags parameter to the open syscall
  static Flags fromOpen(int flags) {
    return {.r = (flags & O_RDONLY) == O_RDONLY || (flags & O_RDWR) == O_RDWR,
            .w = (flags & O_WRONLY) == O_WRONLY || (flags & O_RDWR) == O_RDWR,
            .nofollow = (flags & O_NOFOLLOW) == O_NOFOLLOW,
            .truncate = (flags & O_TRUNC) == O_TRUNC,
            .create = (flags & O_CREAT) == O_CREAT,
            .exclusive = (flags & O_EXCL) == O_EXCL};
  }

  /// Create a Flags instance from the mode and flags parameters to the access syscall
  static Flags fromAccess(int mode, int flags) {
    return {.r = (mode & R_OK) == R_OK,
            .w = (mode & W_OK) == W_OK,
            .x = (mode & X_OK) == X_OK,
            .nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW};
  }

  /// Create a Flags instance from the flags parameter to the stat syscall
  static Flags fromStat(int flags) {
    return {.nofollow = (flags & AT_SYMLINK_NOFOLLOW) == AT_SYMLINK_NOFOLLOW};
  }

  /// Print a Flags struct to an output stream
  friend ostream& operator<<(ostream& o, const Flags& f) {
    return o << (f.r ? 'r' : '-') << (f.w ? 'w' : '-') << (f.x ? 'x' : '-')
             << (f.nofollow ? " nofollow" : "") << (f.truncate ? " truncate" : "")
             << (f.create ? " create" : "") << (f.exclusive ? " exclusive" : "");
  }
};
