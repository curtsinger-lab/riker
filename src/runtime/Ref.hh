#pragma once

#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "artifacts/Artifact.hh"
#include "data/AccessFlags.hh"
#include "runtime/Resolution.hh"
#include "util/UniqueID.hh"

using std::map;
using std::optional;
using std::ostream;
using std::string;

class Build;
class Command;

/***
 * A Ref instance is a bit like a register; it is serialized as the destination where a
 * resolved reference will be saved. An IR Step that resolves a reference will also have a Ref
 * pointer where it will store the resolution result. When serialized, Refs hold no data; the
 * only important aspect of a Ref in the serialized trace is its identity. One step will
 * resolve a reference and save the outcome in a Ref, and later steps may reference that
 * Ref to modify or compare contents of the resolved artifact.
 */
class Ref final {
 public:
  /// The type for a Ref ID
  using ID = uint32_t;

  /// Default constructor
  Ref() noexcept = default;

  // Disallow Copy
  Ref(const Ref&) = delete;
  Ref& operator=(const Ref&) = delete;

  // Allow Move
  Ref(Ref&&) noexcept = default;
  Ref& operator=(Ref&&) noexcept = default;

  /// Get this Ref's unique ID
  size_t getID() const noexcept { return _id; }

  /// Get a short name for this Ref
  string getName() const noexcept { return "r" + std::to_string(getID()); }

  /// Get the artifact reached via this reference
  shared_ptr<Artifact> getArtifact() const noexcept { return _result.getArtifact(); }

  /// Get the result code returned to this reference
  int getResultCode() const noexcept { return _result.getResultCode(); }

  /// Check if this reference resolved successfully
  bool isResolved() const noexcept { return _result.getResultCode() == SUCCESS; }

  /// Get the resolution result
  Resolution getResolution() const noexcept { return _result; }

  /// Set the artifact or error this reference resolves to
  void resolvesTo(Resolution result, AccessFlags flags) noexcept {
    _result = result;
    _flags = flags;
  }

  /// Get the access flags associated with this Ref
  AccessFlags getFlags() const noexcept { return _flags; }

  /// A command is now using this Ref. Return true if this first use by the given command
  bool addUser(Build& b, shared_ptr<Command> c) noexcept {
    // Increment the total user count
    _total_users++;

    // Increment the command-specific user count
    auto count = ++_users[c];
    return count == 1;
  }

  /// A command is no longer using this Ref. Return true if that was the last use by c
  bool removeUser(Build& b, shared_ptr<Command> c) noexcept {
    ASSERT(_users[c] > 0) << "Attempted to close unknown handle to " << this << " from " << c
                          << " -> " << _result;

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
  int getFD() noexcept {
    ASSERT(isResolved()) << "Cannot set up a file descriptor for an unresolved Ref";
    if (!_fd.has_value()) {
      _fd = getArtifact()->getFD(_flags);
    }
    return _fd.value();
  }

  /// Print a Ref
  ostream& print(ostream& o) const noexcept { return o << getName(); }

  /// Stream print wrapper for Ref references
  friend ostream& operator<<(ostream& o, const Ref& r) noexcept { return r.print(o); }

  /// Stream print wrapper for Ref pointers
  friend ostream& operator<<(ostream& o, const Ref* r) noexcept {
    if (r == nullptr) return o << "<null Ref>";
    return o << *r;
  }

 private:
  /// A unique identifier for this reference result
  UniqueID<Ref> _id;

  /// The outcome of a reference resolution saved in this Ref
  Resolution _result;

  /// Keep the flags used to establish this reference so we know what accesses are permitted
  AccessFlags _flags;

  /// Keep track of which commands are using this Ref
  map<shared_ptr<Command>, size_t> _users;

  /// Keep a running total of all users
  size_t _total_users = 0;

  /// If this Ref has a valid file descriptor, it is saved here
  optional<int> _fd;
};