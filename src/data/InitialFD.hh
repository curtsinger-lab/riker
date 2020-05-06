#pragma once

#include <cstdint>
#include <memory>

using std::shared_ptr;

class Reference;

/**
 * Track information required to set up a file descriptor at the start of a command's execution.
 * This differs from the FDEntry class in that it does not refer to a specific artifact. Instead,
 * the tracing layer will use the saved reference to locate an artifact and inflate this to an
 * FDEntry when a command is launched.
 */
class InitialFD {
 public:
  InitialFD() = default;

  InitialFD(shared_ptr<Reference> ref, bool writable) : _ref(ref), _writable(writable) {}

  shared_ptr<Reference> getReference() const { return _ref; }

  bool isWritable() const { return _writable; }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, InitialFD& fd, const uint32_t version);

 private:
  /// The reference used to locate an artifact that the file descriptor points to
  shared_ptr<Reference> _ref;

  /// Is this file descriptor opened in writable mode?
  bool _writable;
};
