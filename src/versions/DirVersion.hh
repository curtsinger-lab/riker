#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "artifacts/Artifact.hh"
#include "util/serializer.hh"
#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

/**
 * A DirVersion represents some part of the state of a directory. Subclasses of DirVersion can
 * encode complete versions that describe all of the contents of the directory, or partial versions
 * that describe an update to a single entry in the directory.
 */
class DirVersion : public Version, public std::enable_shared_from_this<DirVersion> {
 public:
  /// Try to cast this version to one of its subtypes
  template <class T>
  std::shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Get the entry this directory version references, if any
  virtual std::optional<std::string> getEntry() const noexcept { return std::nullopt; }

 private:
  // Declare fields for serialization
  SERIALIZE_EMPTY();
};

/**
 * A BaseDirVersion encodes the starting state for a directory artifact. Every directory has
 * exactly one of these versions. Any updates to the directory are layered on top of the base
 * version.
 */
class BaseDirVersion : public DirVersion {
 public:
  BaseDirVersion(bool created) noexcept : _created(created) {}

  /// Does the base version represent a newly created directory?
  bool getCreated() const noexcept { return _created; }

  /// Commit this base directory version
  void commit(fs::path path, mode_t mode) noexcept;

  /// Get the name for this version type
  virtual std::string getTypeName() const noexcept override {
    if (_created) {
      return "empty dir";
    } else {
      return "on-disk dir";
    }
  }

  /// Print this directory version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    if (_created) {
      return o << "[dir: empty]";
    } else {
      return o << "[dir: on-disk]";
    }
  }

 private:
  /// Does this version encode the creation of a new directory? If not, accessing entries requires
  /// checks against the actual filesystem.
  bool _created;

  // Create default constructor and declare fields for serialization
  BaseDirVersion() noexcept = default;
  SERIALIZE(BASE(DirVersion), _created);
};

/// An DirEntryVersion version updates a directory with a new entry
class DirEntryVersion : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  DirEntryVersion(std::string entry, std::shared_ptr<Artifact> target) noexcept :
      _entry(entry), _target(target) {}

  /// Get the name of the entry this version links
  std::string getEntryName() const noexcept { return _entry; }

  /// Get the name for this version type
  virtual std::string getTypeName() const noexcept override {
    return (_target ? "+" : "-") + std::string(_entry);
  }

  /// Get the entry this directory version references
  virtual std::optional<std::string> getEntry() const noexcept override { return _entry; }

  /// Get the target of this entry
  std::shared_ptr<Artifact> getTarget() const noexcept { return _target; }

  /// Print a the version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    return o << "[dir: " << (_target ? "link" : "unlink") << " " << _entry << "]";
  }

 private:
  std::string _entry;
  std::shared_ptr<Artifact> _target;
};
