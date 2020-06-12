#pragma once

#include <memory>

#include "core/IR.hh"
#include "util/serializer.hh"

using std::shared_ptr;

class Reference;

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
  FileDescriptor(shared_ptr<Reference> ref, bool writable, bool cloexec = false) noexcept :
      _ref(ref), _writable(writable), _cloexec(cloexec) {}

  /// Get the reference used to open the file descriptor
  const shared_ptr<Reference>& getReference() const noexcept { return _ref; }

  /// Check if the file descriptor should be writable
  bool isWritable() const noexcept { return _writable; }

  /// Check if the file descriptor should be closed on exec
  bool isCloexec() const noexcept { return _cloexec; }

  /// Change the cloexec flag for this descriptor
  void setCloexec(bool c) noexcept { _cloexec = c; }

  /// Print a file descriptor
  friend ostream& operator<<(ostream& o, const FileDescriptor& fd) noexcept {
    o << fd._ref << (fd._cloexec ? " (cloexec)" : "");
    if (fd._ref->isResolved()) o << " -> " << fd._ref->getArtifact();
    return o;
  }

 private:
  /// The reference used to locate an artifact that the file descriptor points to
  shared_ptr<Reference> _ref;

  /// Is this file descriptor opened in writable mode?
  bool _writable;

  /// Is this file descriptor closed on exec calls?
  /// When file descriptors are serialized, it's because they appear in a command's initial file
  /// descriptor table. That means they were not closed on exec by definition.
  bool _cloexec = false;

  // Declare fields for serialization
  SERIALIZE(_ref, _writable);
};
