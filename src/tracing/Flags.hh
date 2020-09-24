#pragma once

#include <ostream>

#include <fcntl.h>
#include <fmt/core.h>
#include <sys/stat.h>
#include <sys/types.h>

using std::ostream;

/****** Pretty Printers for Syscall Arguments ******/

/// A wrapper for file descriptors used as the base in the at* system calls
class at_fd {
 public:
  /// Create an at_fd from a file descriptor
  explicit at_fd(int fd) noexcept : _fd(fd) {}

  /// Create an at_fd that indicates the current working directory
  static at_fd cwd() noexcept { return at_fd(AT_FDCWD); }

  /// Does this fd indiciate the current working directory?
  bool isCWD() const noexcept { return _fd == AT_FDCWD; }

  /// Get the file descriptor number
  int getFD() const noexcept { return _fd; }

  friend ostream& operator<<(ostream& o, const at_fd& v) noexcept {
    if (v._fd == AT_FDCWD) {
      return o << "AT_FDCWD";
    } else {
      return o << v._fd;
    }
  }

 private:
  int _fd;
};

class mode_printer {
 public:
  mode_printer(mode_t mode) : _mode(mode) {}

  friend ostream& operator<<(ostream& o, const mode_printer& p) noexcept {
    if (p._mode == 0) return o << 0;

    o << ((p._mode & S_IRUSR) ? 'r' : '-');
    o << ((p._mode & S_IWUSR) ? 'w' : '-');
    o << ((p._mode & S_IXUSR) ? ((p._mode & S_ISUID) ? 's' : 'x')
                              : ((p._mode & S_ISUID) ? 'S' : '-'));
    o << ((p._mode & S_IRGRP) ? 'r' : '-');
    o << ((p._mode & S_IWGRP) ? 'w' : '-');
    o << ((p._mode & S_IXGRP) ? ((p._mode & S_ISGID) ? 's' : 'x')
                              : ((p._mode & S_ISGID) ? 'S' : '-'));
    o << ((p._mode & S_IROTH) ? 'r' : '-');
    o << ((p._mode & S_IWOTH) ? 'w' : '-');
    o << ((p._mode & S_IXOTH) ? ((p._mode & S_ISVTX) ? 't' : 'x')
                              : ((p._mode & S_ISVTX) ? 'T' : '-'));

    o << fmt::format(" ({:o})", p._mode);
    return o;
  }

 private:
  mode_t _mode;
};

/// A wrapper for O_* flags provided to system calls
class o_flags {
 public:
  /// Create a default o_flags value
  o_flags() noexcept : _flags(0) {}

  /// Create a wrapper for O_* flags from an integer value
  explicit o_flags(int flags) noexcept : _flags(flags) {}

  /// Check if the flags include specific option
  template <int flag>
  bool has() const noexcept {
    return (_flags & flag) == flag;
  }

  /// Do the flags include a request for read access?
  bool readable() const noexcept { return has<O_RDWR>() || (has<O_RDONLY>() && !has<O_WRONLY>()); }

  /// Do teh flags include a request for write access?
  bool writable() const noexcept { return has<O_RDWR>() || has<O_WRONLY>(); }

  friend ostream& operator<<(ostream& o, const o_flags& p) noexcept {
    bool noflag = true;

    // decode O_RDWR, O_RDONLY, O_WRONLY. Check O_RDWR first in case O_RDWR == O_RDONLY | O_WRONLY
    if ((p._flags & O_RDWR) == O_RDWR) {
      o << "O_RDWR";
      noflag = false;
    } else if ((p._flags & O_WRONLY) == O_WRONLY) {
      o << "O_WRONLY";
      noflag = false;
    } else if ((p._flags & O_RDONLY) == O_RDONLY) {
      o << "O_RDONLY";
      noflag = false;
    }

    // pretty printer
    auto dec = [&](int flag, const char* fstr) {
      if ((p._flags & flag) == flag) {
        if (!noflag) o << "|";
        o << fstr;
        noflag = false;
      }
    };

    // decode the rest
    dec(O_CLOEXEC, "O_CLOEXEC");
    dec(O_CREAT, "O_CREAT");
    dec(O_DIRECT, "O_DIRECT");
    dec(O_DIRECTORY, "O_DIRECTORY");
    dec(O_EXCL, "O_EXCL");
    dec(O_NOCTTY, "O_NOCTTY");
    dec(O_NOFOLLOW, "O_NOFOLLOW");
    dec(O_NONBLOCK, "O_NONBLOCK");
    dec(O_TMPFILE, "O_TMPFILE");
    dec(O_TRUNC, "O_TRUNC");

    // append flags in octal
    o << fmt::format(" ({:o})", p._flags);

    return o;
  }

 private:
  int _flags;
};

class at_flags_printer {
 public:
  at_flags_printer(int flags) : _flags(flags) {}

  friend ostream& operator<<(ostream& o, const at_flags_printer& p) noexcept {
    if (p._flags == 0) return o << 0;

    bool noflag = true;

    // pretty printer
    auto dec = [&](int flag, const char* fstr) {
      if ((p._flags & flag) == flag) {
        if (!noflag) o << "|";
        o << fstr;
        noflag = false;
      }
    };

    // Decode the flags
    dec(AT_EACCESS, "AT_EACESS");
    dec(AT_EMPTY_PATH, "AT_EMPTY_PATH");
    dec(AT_SYMLINK_FOLLOW, "AT_SYMLINK_FOLLOW");
    dec(AT_SYMLINK_NOFOLLOW, "AT_SYMLINK_NOFOLLOW");
    dec(AT_REMOVEDIR, "AT_REMOVEDIR");

    return o << fmt::format(" ({:o})", p._flags);
  }

 private:
  int _flags;
};

class rename_flags_printer {
 public:
  rename_flags_printer(int flags) : _flags(flags) {}

  friend ostream& operator<<(ostream& o, const rename_flags_printer& p) noexcept {
    bool noflag = true;

    // pretty printer
    auto dec = [&](int flag, const char* fstr) {
      if ((p._flags & flag) == flag) {
        if (!noflag) o << "|";
        o << fstr;
        noflag = false;
      }
    };

    // Decode the flags
    dec(RENAME_EXCHANGE, "RENAME_EXCHANGE");
    dec(RENAME_NOREPLACE, "RENAME_NOREPLACE");
    dec(RENAME_WHITEOUT, "RENAME_WHITEOUT");

    return o << fmt::format(" ({:o})", p._flags);
  }

 private:
  int _flags;
};
