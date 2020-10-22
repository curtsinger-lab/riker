#include "Ref.hh"

#include "artifacts/Artifact.hh"
#include "runtime/Build.hh"

/// A command is now using this Ref. Return true if this first use by the given command
bool Ref::addUser(Build& b, shared_ptr<Command> c) noexcept {
  // Increment the total user count
  _total_users++;

  // Increment the command-specific user count
  auto count = ++_users[c];
  return count == 1;
}

/// A command is no longer using this Ref. Return true if that was the last use by c
bool Ref::removeUser(Build& b, shared_ptr<Command> c) noexcept {
  ASSERT(_users[c] > 0) << "Attempted to close unknown handle to " << this << " from " << c
                        << " -> " << getErrorName(_rc);

  // Decrement the total user count
  _total_users--;

  // If this was the last user and we have a file descriptor open, close it
  if (_total_users == 0 && _fd.has_value()) {
    ::close(_fd.value());
  }

  // Decrement the command-specific user count
  auto count = --_users[c];
  return count == 0;
}

/// Get a file descriptor for this Ref
int Ref::getFD() noexcept {
  ASSERT(isResolved()) << "Cannot set up a file descriptor for an unresolved Ref";
  if (!_fd.has_value()) {
    _fd = getArtifact()->getFD(_flags);
  }
  return _fd.value();
}