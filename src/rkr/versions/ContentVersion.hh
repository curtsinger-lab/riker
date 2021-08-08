#pragma once

#include <filesystem>
#include <memory>
#include <ostream>

#include "util/log.hh"
#include "util/stats.hh"
#include "versions/Version.hh"

namespace fs = std::filesystem;

class Command;

/// The fingerprint polciy determines what type of fingerprint we collect for a version.
///  None: no fingerprint at all
///  Quick: mtime only
///  Full: a hash of the content
enum class FingerprintType { None, Quick, Full };

class ContentVersion : public Version, public std::enable_shared_from_this<ContentVersion> {
 public:
  /// The type of a ContentVersion ID
  using ID = uint32_t;

  // Default constructor
  ContentVersion() noexcept = default;

  // Disallow Copy
  ContentVersion(const ContentVersion&) = delete;
  ContentVersion& operator=(const ContentVersion&) = delete;

  // Allow Move
  ContentVersion(ContentVersion&&) noexcept = default;
  ContentVersion& operator=(ContentVersion&&) noexcept = default;

  /// Try to cast this version to one of its subtypes
  template <class T>
  std::shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Check if this version can be committed
  virtual bool canCommit() const noexcept {
    // Versions are unsaved by default
    return false;
  }

  /// Check if this version matches another
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept {
    return other.get() == this;
  }

  /// Can a write of this version be coalesced with another?
  virtual bool canCoalesceWith(std::shared_ptr<ContentVersion> other) const noexcept {
    return false;
  }

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept {
    // do nothing by default
  }

  std::optional<ContentVersion::ID> getID(size_t buffer_id) {
    if (_buffer_id == buffer_id) return _id;
    return std::nullopt;
  }

  void setID(size_t buffer_id, ContentVersion::ID id) {
    _buffer_id = buffer_id;
    _id = id;
  }

 protected:
  ContentVersion::ID _id;
  size_t _buffer_id;
};