#pragma once

#include <memory>

#include "data/AccessFlags.hh"
#include "runtime/RefResult.hh"
#include "util/serializer.hh"

using std::shared_ptr;

class Ref;

/**
 * Track information about a file descriptor.
 * This class is used for all file descriptors during tracing, and is serialized with commands to
 * store the set of file descriptors that must be in place when a command is launched.
 */
class FileDescriptor {
 public:
  /// Default constructor, used only for serialization
  FileDescriptor() noexcept = default;

  /// Create a record of an initial file descriptor
  explicit FileDescriptor(shared_ptr<RefResult> ref, bool cloexec = false) noexcept :
      _ref(ref), _cloexec(cloexec) {}

  /// Get the reference used to open the file descriptor
  shared_ptr<RefResult> getRef() const noexcept { return _ref; }

  /// Check if the file descriptor should be writable
  bool isWritable() const noexcept { return _ref->getFlags().w; }

  /// Get the access flags
  AccessFlags getFlags() const noexcept { return _ref->getFlags(); }

  /// Check if the file descriptor should be closed on exec
  bool isCloexec() const noexcept { return _cloexec; }

  /// Change the cloexec flag for this descriptor
  void setCloexec(bool c) noexcept { _cloexec = c; }

  /// Print a file descriptor
  friend ostream& operator<<(ostream& o, const FileDescriptor& fd) noexcept {
    o << fd._ref << (fd._cloexec ? " (cloexec)" : "") << " -> " << fd._ref->getResolution();
    return o;
  }

 private:
  /// The reference used to locate an artifact that the file descriptor points to
  shared_ptr<RefResult> _ref;

  /// Is this file descriptor closed on exec calls?
  /// When file descriptors are serialized, it's because they appear in a command's initial file
  /// descriptor table. That means they were not closed on exec by definition.
  bool _cloexec = false;
};
