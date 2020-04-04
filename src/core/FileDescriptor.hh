#pragma once

#include <memory>
#include <ostream>

using std::ostream;
using std::shared_ptr;

class Reference;
class Artifact;

class FileDescriptor {
 public:
  /// Create an object to track a file descriptor
  FileDescriptor(shared_ptr<Reference> ref, shared_ptr<Artifact> artifact, bool writable,
                 bool cloexec = false) :
      _ref(ref), _artifact(artifact), _writable(writable), _cloexec(cloexec) {}

  /// Get the reference used to construct this file descriptor
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the artifact this file descriptor points to
  shared_ptr<Artifact> getArtifact() const { return _artifact; }

  /// Check if this file descriptor is writable
  bool isWritable() const { return _writable; }

  /// Check if this file descriptor is closed on exec
  bool isCloexec() const { return _cloexec; }

  /// Change the cloexec flag for this descriptor
  void setCloexec(bool c) { _cloexec = c; }

  /// Print a file descriptor
  friend ostream& operator<<(ostream& o, const FileDescriptor& fd) {
    return o << fd._ref << (fd._cloexec ? " (cloexec)" : "");
  }

 private:
  shared_ptr<Reference> _ref;      //< The reference used to construct this file descriptor
  shared_ptr<Artifact> _artifact;  //< The artifact referred to via this file descriptor
  bool _writable = false;          //< Is this file descriptor writable?
  bool _cloexec = false;           //< Should this descriptor be closed on an exec call?
};
