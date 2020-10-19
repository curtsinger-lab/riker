#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>

#include "runtime/Resolution.hh"
#include "util/UniqueID.hh"

using std::map;
using std::ostream;
using std::string;

class Build;
class Command;

/***
 * A RefResult instance is a bit like a register; it is serialized as the destination where a
 * resolved reference will be saved. An IR Step that resolves a reference will also have a RefResult
 * pointer where it will store the resolution result. When serialized, RefResults hold no data; the
 * only important aspect of a RefResult in the serialized trace is its identity. One step will
 * resolve a reference and save the outcome in a RefResult, and later steps may reference that
 * RefResult to modify or compare contents of the resolved artifact.
 */
class RefResult final {
 public:
  /// The type for a RefResult ID
  using ID = uint32_t;

  /// Default constructor
  RefResult() noexcept = default;

  // Disallow Copy
  RefResult(const RefResult&) = delete;
  RefResult& operator=(const RefResult&) = delete;

  // Allow Move
  RefResult(RefResult&&) noexcept = default;
  RefResult& operator=(RefResult&&) noexcept = default;

  /// Get this RefResult's unique ID
  size_t getID() const noexcept { return _id; }

  /// Get a short name for this RefResult
  string getName() const noexcept { return "r" + std::to_string(getID()); }

  /// Get the artifact reached via this reference
  shared_ptr<Artifact> getArtifact() const noexcept { return _result.getArtifact(); }

  /// Get the result code returned to this reference
  int getResultCode() const noexcept { return _result.getResultCode(); }

  /// Check if this reference resolved successfully
  bool isResolved() const noexcept { return _result.getResultCode() == SUCCESS; }

  /// Get the resolution result
  Resolution getResolution() const noexcept { return _result; }

  /// Save a resolution result in this RefResult
  void resolvesTo(Resolution r) noexcept { _result = r; }

  /// A command is now using this RefResult. Return true if this first use by the given command
  bool addUser(Build& b, shared_ptr<Command> c) noexcept {
    auto count = ++_users[c];
    return count == 1;
  }

  /// A command is no longer using this RefResult. Return true if that was the last use by c
  bool removeUser(Build& b, shared_ptr<Command> c) noexcept {
    ASSERT(_users[c] > 0) << "Attempted to close unknown handle to " << this << " from " << c
                          << " -> " << _result;

    auto count = --_users[c];
    return count == 0;
  }

  /// Print a RefResult
  ostream& print(ostream& o) const noexcept { return o << getName(); }

  /// Stream print wrapper for RefResult references
  friend ostream& operator<<(ostream& o, const RefResult& r) noexcept { return r.print(o); }

  /// Stream print wrapper for RefResult pointers
  friend ostream& operator<<(ostream& o, const RefResult* r) noexcept {
    if (r == nullptr) return o << "<null RefResult>";
    return o << *r;
  }

 private:
  /// A unique identifier for this reference result
  UniqueID<RefResult> _id;

  /// The outcome of a reference resolution saved in this RefResult
  Resolution _result;

  /// Keep track of which commands are using this RefResult
  map<shared_ptr<Command>, size_t> _users;
};