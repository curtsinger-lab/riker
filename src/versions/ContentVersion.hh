#pragma once

#include <filesystem>
#include <memory>
#include <ostream>

#include "ui/stats.hh"
#include "util/log.hh"
#include "util/serializer.hh"
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
  /// Try to cast this version to one of its subtypes
  template <class T>
  std::shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Get the command that created this version
  std::shared_ptr<Command> getCreator() const noexcept { return _creator.lock(); }

  /// Record that this version was created by command c
  void createdBy(std::shared_ptr<Command> c) noexcept { _creator = c; }

  /// Check if this version has been committed
  bool isCommitted() const noexcept { return _committed; }

  /// Mark this version as committed
  void setCommitted(bool committed = true) noexcept { _committed = committed; }

  /// Save a copy of this version for later reuse. Inform the provided IRSink of the save.
  virtual void cache(fs::path path) noexcept {}

  /// Commit this version to the filesystem
  virtual void commit(fs::path path) noexcept = 0;

  /// Check if this version can be committed
  virtual bool canCommit() const noexcept {
    // Versions are unsaved by default
    return false;
  }

  /// Save a fingerprint of this version for later comparison. If a new fingerprint is saved, inform
  /// the provided IRSink.
  virtual void fingerprint(fs::path path, FingerprintType type) noexcept {
    // By default, fingerprinting a version just saves it
    cache(path);
  }

  /// Check if this version matches another
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept {
    FAIL << "Unsupported call to matches() on version " << this;
    return false;
  }

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept {
    // do nothing by default
  }

 protected:
  // Declare fields for serialization
  SERIALIZE_EMPTY();

  /******** Transient Fields *********/

  /// Has this version been committed?
  bool _committed = false;

  /// The command that created this version
  std::weak_ptr<Command> _creator;
};