#pragma once

#include <memory>
#include <ostream>
#include <string>

#include "build/Resolution.hh"
#include "util/UniqueID.hh"
#include "util/serializer.hh"

using std::ostream;
using std::string;

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

  /// Get the resolution result
  Resolution getResult() const noexcept { return _result; }

  /// Save a resolution result in this RefResult
  void resolvesTo(Resolution r) noexcept { _result = r; }

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
  /// RefResults are serialized, but we only track their identity. All other fields are transient.
  SERIALIZE_EMPTY();

  /// A unique identifier for this reference result
  UniqueID<RefResult> _id;

  /// The outcome of a reference resolution saved in this RefResult
  Resolution _result;
};