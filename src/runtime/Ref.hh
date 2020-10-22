#pragma once

#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "data/AccessFlags.hh"
#include "util/UniqueID.hh"
#include "util/log.hh"
#include "util/wrappers.hh"

using std::map;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

class Artifact;
class Build;
class Command;

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int8_t { SUCCESS = 0 };

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
  Ref() noexcept {}

  /// Create a Ref that resolves to an error code
  Ref(int rc) noexcept : _rc(rc) {
    ASSERT(rc != SUCCESS)
        << "Attempted to create a Ref that resolved successfully without providing an artifact";
  }

  /// Create a Ref that resolves to an artifact
  Ref(AccessFlags flags, shared_ptr<Artifact> artifact) noexcept :
      _rc(SUCCESS), _artifact(artifact), _flags(flags) {
    ASSERT(artifact) << "Attempted to create a Ref that resolved successfully with a null artifact";
  }

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
  shared_ptr<Artifact> getArtifact() const noexcept { return _artifact.lock(); }

  /// Get the result code returned to this reference
  int getResultCode() const noexcept { return _rc; }

  /// Check if this Ref resolved successfully
  bool isSuccess() const noexcept { return _rc == SUCCESS; }

  /// Check if this reference resolved successfully
  bool isResolved() const noexcept { return _rc == SUCCESS; }

  /// Get the access flags associated with this Ref
  AccessFlags getFlags() const noexcept { return _flags; }

  /// A command is now using this Ref. Return true if this first use by the given command
  bool addUser(Build& b, shared_ptr<Command> c) noexcept;

  /// A command is no longer using this Ref. Return true if that was the last use by c
  bool removeUser(Build& b, shared_ptr<Command> c) noexcept;

  /// Get a file descriptor for this Ref
  int getFD() noexcept;

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

  /// The error code (or SUCCESS) that this reference resolved to
  int _rc;

  /// The artifact this reference resolved to
  weak_ptr<Artifact> _artifact;

  /// Keep the flags used to establish this reference so we know what accesses are permitted
  AccessFlags _flags;

  /// Keep track of which commands are using this Ref
  map<shared_ptr<Command>, size_t> _users;

  /// Keep a running total of all users
  size_t _total_users = 0;

  /// If this Ref has a valid file descriptor, it is saved here
  optional<int> _fd;
};