#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>

#include <cereal/access.hpp>

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;
using std::weak_ptr;

class Command;
class Reference;

using Metadata = struct stat;
using Fingerprint = struct stat;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  Version() = default;

  virtual ~Version() = default;

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Record a version that follows this one
  void followedBy(shared_ptr<Version> v);

  /// Get the first version of this artifact
  shared_ptr<Version> getFirstVersion();

  /// Get the latest version of this artifact
  shared_ptr<Version> getLatestVersion();

  /// Get the number of preceding versions of this artifact
  size_t getVersionNumber() const { return _previous ? (_previous->getVersionNumber() + 1) : 0; }

  /// Get the previous version
  shared_ptr<Version> getPrevious() const { return _previous; }

  /// Get the next version
  shared_ptr<Version> getNext() const { return _next.lock(); }

  /// Get the path for this version if it has one
  virtual optional<string> getPath() const { return _previous ? _previous->getPath() : nullopt; }

  /// Get the command that created this version
  virtual shared_ptr<Command> getCreator() const { return nullptr; }

  /// Is this version saved in a way that allows us to reproduce it?
  virtual bool isSaved() const { return false; }

  /// Do we have saved metadata for this version?
  virtual bool hasMetadata() const { return false; }

  /// Save the metadata for this version
  virtual void saveMetadata() {}

  /// Get saved metadata for this version
  virtual optional<Metadata> getMetadata() const { return nullopt; }

  /// Compare metadata for this version to another version
  virtual bool metadataMatch(shared_ptr<Version> other) const;

  /// Do we have a fingerprint for the contents of this version?
  virtual bool hasFingerprint() const { return false; }

  /// Save a fingerprint of this version's contents
  virtual void saveFingerprint() {}

  /// Get the saved fingerprint for this version
  virtual optional<Fingerprint> getFingerprint() const { return nullopt; }

  /// Compare the fingerprint for this version to another version
  virtual bool fingerprintMatch(shared_ptr<Version> other) const;

  /// Check if this version has been accessed
  bool isAccessed() const { return _accessed; }

  /// Mark this version as accessed
  void setAccessed() { _accessed = true; }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Version& v, const uint32_t version);

 private:
  /// The version that preceded this one, if any
  shared_ptr<Version> _previous;

  /// The version that comes after this one, if any
  weak_ptr<Version> _next;

  /// Transient: has this version been accessed?
  bool _accessed;
};

class InitialPipeVersion : public Version {
 private:
  friend class cereal::access;
  InitialPipeVersion() = default;

 public:
  InitialPipeVersion(shared_ptr<Command> creator) : _creator(creator) {}

  virtual shared_ptr<Command> getCreator() const override { return _creator; }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, InitialPipeVersion& v, const uint32_t version);

 private:
  shared_ptr<Command> _creator;
};

class OpenedVersion : public Version {
 private:
  friend class cereal::access;
  OpenedVersion() = default;

 public:
  OpenedVersion(shared_ptr<Reference> ref) : _ref(ref) {}

  virtual optional<string> getPath() const override;

  virtual bool hasMetadata() const override { return _metadata.has_value(); }

  virtual void saveMetadata() override;

  virtual optional<Metadata> getMetadata() const override { return _metadata; }

  virtual void saveFingerprint() override { saveMetadata(); }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, OpenedVersion& v, const uint32_t version);

 private:
  shared_ptr<Reference> _ref;
  optional<Metadata> _metadata;
};

class ModifiedVersion : public Version {
 private:
  friend class cereal::access;
  ModifiedVersion() = default;

 public:
  ModifiedVersion(shared_ptr<Command> creator, shared_ptr<Reference> ref) :
      _creator(creator), _ref(ref) {}

  virtual shared_ptr<Command> getCreator() const override { return _creator; }

  virtual bool hasMetadata() const override { return _metadata.has_value(); }

  virtual void saveMetadata() override;

  virtual optional<Metadata> getMetadata() const override { return _metadata; }

  virtual void saveFingerprint() override { saveMetadata(); }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, ModifiedVersion& v, const uint32_t version);

 private:
  shared_ptr<Command> _creator;
  shared_ptr<Reference> _ref;
  optional<Metadata> _metadata;
};
