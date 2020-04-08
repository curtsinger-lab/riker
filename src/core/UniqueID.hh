#pragma once

#include <optional>

using std::optional;

/**
 * This is a utility class for assigning unique identifiers to instaces. Identifiers are assigned
 * lazily (on conversion to size_t), so only the instances that are actually used will have IDs, and
 * those IDs should be sequential.
 *
 * Identifiers are unique within a given domain. If you are assigning IDs to a non-polymorphic type,
 * Domain is typically the type of the class being identified. When dealing with polymorphism,
 * passing in a parent class for Domain guarantees that IDs are unique across all subtypes of that
 * parent class.
 */
template <class Domain>
class UniqueID {
 public:
  // Default constructor
  UniqueID() = default;

  // Copy constructor must NOT copy the unique ID
  UniqueID(const UniqueID&) : _assigned_id() {}

  // Move constructor can move the ID
  UniqueID(UniqueID&&) = default;

  // Copy assignment must NOT copy the unique ID
  UniqueID& operator=(const UniqueID&) {}

  // Move assignment can move the unique ID
  UniqueID& operator=(UniqueID&&) = default;

  operator size_t() const {
    // If an ID hasn't been assigned, assign one now
    if (!_assigned_id) {
      // We need to violate the const-ness of this ID to assign a value
      const_cast<UniqueID*>(this)->_assigned_id = getNextID();
    }
    return _assigned_id.value();
  }

 private:
  optional<size_t> _assigned_id;

  static size_t getNextID() {
    static size_t next_id = 0;
    return next_id++;
  }
};
