#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>

#include "util/log.hh"

using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

class Artifact;

// Add a success constant so we don't have to keep returning 0 as a magic number
enum : int8_t { SUCCESS = 0 };

// Set up a map from return codes to names
inline static map<int8_t, string> errors = {
    {SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
    {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
    {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}, {ENOTDIR, "ENOTDIR"}};

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

  /// Get the artifact reached via this resolution
  shared_ptr<Artifact> getArtifact() const noexcept { return _artifact.lock(); }

  /// Coerce this resolution result to a boolean
  operator bool() const noexcept { return static_cast<bool>(_artifact.lock()); }

  /// Coerce this resolution result to an artifact pointer
  operator shared_ptr<Artifact>() const noexcept { return _artifact.lock(); }

  /// Coerce this resolution result to an int
  operator int() const noexcept { return _rc; }

  /// Compare this resolution to an int
  bool operator==(int rc) const noexcept { return _rc == rc; }

  /// Compare this resolution to an int
  bool operator!=(int rc) const noexcept { return _rc != rc; }

  /// Access the artifact in this result as a pointer
  shared_ptr<Artifact> operator->() const noexcept { return _artifact.lock(); }

  /// Print the result of a resolution
  friend ostream& operator<<(ostream& o, const Resolution& r) noexcept {
    auto a = r._artifact.lock();
    if (a) {
      return o << a;
    } else {
      return o << errors[r._rc];
    }
  }

 private:
  weak_ptr<Artifact> _artifact;
  int _rc;
};