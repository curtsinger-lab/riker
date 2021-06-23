#pragma once

#include <ostream>
#include <string>

#include "util/serializer.hh"
#include "util/stats.hh"

class Version {
 public:
  // Increment the version count on creation
  Version() noexcept { stats::versions++; }

  // Default virtual destructor in case a subclass wants to override the destructor
  virtual ~Version() noexcept = default;

  /// Try to cast this version to some subtype
  template <class T>
  bool is_a() const noexcept {
    if (dynamic_cast<const T*>(this)) return true;
    return false;
  }

  /// Get the name for the type of version this is
  virtual std::string getTypeName() const noexcept = 0;

  /// Print this version
  virtual std::ostream& print(std::ostream& o) const noexcept = 0;

  /// Print a Version
  friend std::ostream& operator<<(std::ostream& o, const Version& v) noexcept { return v.print(o); }

  /// Print a Version*
  friend std::ostream& operator<<(std::ostream& o, const Version* v) noexcept {
    if (v == nullptr) return o << "<null Version>";
    return v->print(o);
  }
};