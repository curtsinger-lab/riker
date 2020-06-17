#pragma once

#include <memory>

#include "util/log.hh"

using std::dynamic_pointer_cast;
using std::shared_ptr;

class Artifact;

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int8_t { SUCCESS = 0 };

class Resolution {
 public:
  Resolution() = default;

  /// Create a resolution result that successfully produced an artifact
  template <class T>
  Resolution(shared_ptr<T> artifact) noexcept : _artifact(artifact), _rc(SUCCESS) {
    ASSERT(artifact) << "Resolution succeeded, but returned a null artifact";
  }

  /// Create a resolution result that returned with an error code
  Resolution(int rc) noexcept : _rc(rc) {
    ASSERT(rc != SUCCESS) << "Resolution failed, but produced return code SUCCESS";
  }

  /// Coerce this resolution result to a boolean
  operator bool() const noexcept { return static_cast<bool>(_artifact); }

  /// Coerce this resolution result to an artifact pointer
  operator shared_ptr<Artifact>() const noexcept { return _artifact; }

  /// Coerce this resolution result to an int
  operator int() const noexcept { return _rc; }

  /// Compare this resolution to an int
  bool operator==(int rc) const noexcept { return _rc == rc; }

  /// Compare this resolution to an int
  bool operator!=(int rc) const noexcept { return _rc != rc; }

  /// Access the artifact in this result as a pointer
  shared_ptr<Artifact> operator->() const noexcept { return _artifact; }

  /// Attempt to cast the artifact to a specific artifact type
  template <class T>
  shared_ptr<T> as() const noexcept {
    return dynamic_pointer_cast<T>(_artifact);
  }

 private:
  shared_ptr<Artifact> _artifact;
  int _rc;
};