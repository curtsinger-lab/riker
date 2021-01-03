#pragma once

#include <memory>
#include <ostream>

#include "ui/stats.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::ostream;
using std::shared_ptr;
using std::weak_ptr;

class CommandRun;

class ContentVersion : public std::enable_shared_from_this<ContentVersion> {
 public:
  ContentVersion() noexcept { stats::versions++; }

  virtual ~ContentVersion() noexcept = default;

  // Disallow Copy
  ContentVersion(const ContentVersion&) = delete;
  ContentVersion& operator=(const ContentVersion&) = delete;

  // Allow Move
  ContentVersion(ContentVersion&&) noexcept = default;
  ContentVersion& operator=(ContentVersion&&) noexcept = default;

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
  virtual bool matches(shared_ptr<ContentVersion> other) const noexcept {
    FAIL << "Unsupported call to matches() on version " << this;
    return false;
  }

  /// Get the name for the type of version this is
  virtual string getTypeName() const noexcept = 0;

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept = 0;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const ContentVersion& v) noexcept { return v.print(o); }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const ContentVersion* v) noexcept {
    if (v == nullptr) return o << "<null ContentVersion>";
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