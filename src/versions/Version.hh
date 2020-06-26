#pragma once

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>

#include "artifacts/Artifact.hh"
#include "util/serializer.hh"

using std::optional;
using std::ostream;
using std::pair;
using std::shared_ptr;
using std::string;

class Artifact;
class Reference;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  Version() noexcept = default;

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
  shared_ptr<Command> getCreator() const noexcept { return _creator.lock(); }

  /// Record that this version was created by command c
  void createdBy(shared_ptr<Command> c) noexcept { _creator = c; }

  /// Check if this version has been accessed
  bool isAccessed() const noexcept { return _accessed; }

  /// Record that this version has been accessed
  void accessed() noexcept { _accessed = true; }

  /// Check if this version has been committed
  bool isCommitted() const noexcept { return _committed; }

  /// Mark this version as committed
  void setCommitted() noexcept { _committed = true; }

  /// Can this version be committed? If it is already committed, this should return true
  virtual bool canCommit() const noexcept = 0;

  /// Commit this version
  virtual void commit(fs::path path) noexcept = 0;

  /// Get the name for the type of version this is
  virtual string getTypeName() const noexcept = 0;

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept = 0;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) noexcept { return v.print(o); }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) noexcept { return v->print(o); }

 protected:
  // Declare fields for serialization
  SERIALIZE_EMPTY();

  /******** Transient Fields *********/

  /// Has this version been committed?
  bool _committed = false;

  /// The command that created this version
  weak_ptr<Command> _creator;

  /// Has this version been accessed by any command?
  bool _accessed = false;
};
