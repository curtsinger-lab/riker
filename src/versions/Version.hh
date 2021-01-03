#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <string>

#include "ui/stats.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

class Artifact;
class Build;
class Command;
class CommandRun;
class IRSink;

namespace fs = std::filesystem;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  Version() noexcept { stats::versions++; }

  virtual ~Version() noexcept = default;

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) noexcept = default;
  Version& operator=(Version&&) noexcept = default;

  /// Try to cast this version to one of its subtypes
  template <class T>
  shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Get the command that created this version
  shared_ptr<CommandRun> getCreator() const noexcept { return _creator.lock(); }

  /// Record that this version was created by command c
  void createdBy(shared_ptr<CommandRun> r) noexcept { _creator = r; }

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
  virtual void fingerprint(fs::path path) noexcept {
    // By default, fingerprinting a version just saves it
    cache(path);
  }

  /// Check if this version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept {
    // Match is not implemented by default
    FAIL << "Un-implemented comparison of versions " << this << " and " << other;
    return false;
  }

  /// Tell the garbage collector to preserve this version.
  virtual void gcLink() noexcept {
    // do nothing by default
  }

  /// Get the name for the type of version this is
  virtual string getTypeName() const noexcept = 0;

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept = 0;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) noexcept { return v.print(o); }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) noexcept {
    if (v == nullptr) return o << "<null Version>";
    return v->print(o);
  }

 protected:
  // Declare fields for serialization
  SERIALIZE_EMPTY();

  /******** Transient Fields *********/

  /// Has this version been committed?
  bool _committed = false;

  /// The command run that created this version
  weak_ptr<CommandRun> _creator;
};
