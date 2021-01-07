#include "Ref.hh"

#include <unistd.h>

#include "artifacts/Artifact.hh"

/// A command is now using this Ref. Return true if this first use by the given command
void Ref::addUser() noexcept {
  // Increment the total user count
  _users++;
}

/// A command is no longer using this Ref. Return true if that was the last use by c
void Ref::removeUser() noexcept {
  ASSERT(_users > 0) << "Attempted to close unknown handle to " << this;

  // Decrement the total user count
  _users--;

  // If this was the last user and we have a file descriptor open, close it
  if (_users == 0 && _fd.has_value()) {
    ::close(_fd.value());
  }
}

/// Get a file descriptor for this Ref
int Ref::getFD() noexcept {
  ASSERT(isResolved()) << "Cannot set up a file descriptor for an unresolved Ref";
  if (!_fd.has_value()) {
    _fd = getArtifact()->getFD(_flags);
  }
  return _fd.value();
}