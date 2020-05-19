#pragma once

#include <cstdint>
#include <memory>

#include "util/serializer.hh"

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
  /// Default constructor, used only for serialization
  InitialFD() = default;

  /// Create a record of an initial file descriptor
  InitialFD(shared_ptr<Reference> ref, bool writable) : _ref(ref), _writable(writable) {}

  /// Get the reference used to open the file descriptor
  shared_ptr<Reference> getReference() const { return _ref; }

  /// Check if the file descriptor should be writable
  bool isWritable() const { return _writable; }

 private:
  /// The reference used to locate an artifact that the file descriptor points to
  shared_ptr<Reference> _ref;

  /// Is this file descriptor opened in writable mode?
  bool _writable;

  // Declare fields for serialization
  SERIALIZE(_ref, _writable);
};
