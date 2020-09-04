#pragma once

#include <optional>

using std::optional;

/**
 * This is a utility class for assigning unique identifiers to instaces. Identifiers are assigned
 * sequentially on creation.
 *
 * Identifiers are unique within a given domain. If you are assigning IDs to a non-polymorphic type,
 * Domain is typically the type of the class being identified. When dealing with polymorphism,
 * passing in a parent class for Domain guarantees that IDs are unique across all subtypes of that
 * parent class.
 */
template <class Domain>
class UniqueID {
 public:
  // Default constructor assigns an ID
  UniqueID() noexcept {}

  // Copy constructor assigns a new ID to the copy
  UniqueID(const UniqueID&) noexcept : _assigned(false) {}

  // Move constructor can move the ID
  UniqueID(UniqueID&&) noexcept = default;

  // Copy assignment assigns a new ID to the copy
  UniqueID& operator=(const UniqueID&) noexcept { _assigned = false; }

  // Move assignment can move the unique ID
  UniqueID& operator=(UniqueID&&) noexcept = default;

  // Get the assigned ID
  operator size_t() const noexcept {
    if (!_assigned) {
      _assigned_id = getNextID();
      _assigned = true;
    }
    return _assigned_id;
  }

 private:
  mutable bool _assigned = false;
  mutable size_t _assigned_id;

  static size_t getNextID() noexcept {
    static size_t next_id = 0;
    return next_id++;
  }
};
