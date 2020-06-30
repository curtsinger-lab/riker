#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <ostream>
#include <set>

#include "build/Resolution.hh"
#include "core/IR.hh"
#include "util/log.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::nullopt;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;

namespace fs = std::filesystem;

class Env;

/// Base class for all of the various types of directory versions
class DirVersion : public Version {
 public:
  /**
   * Check to see if this directory version guarantees the presence or absence of a named entry.
   * A yes or no answer is definite, but partial versions can return "maybe", indicating that
   * checking should continue on to additional version.
   */
  virtual optional<Resolution> getEntry(Env& env,
                                        shared_ptr<DirArtifact> dir,
                                        string name) noexcept = 0;

  /// Add this version's contributions to a map of directory entries
  virtual void getKnownEntries(map<string, shared_ptr<Artifact>>& entries) noexcept = 0;

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept = 0;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept = 0;

 private:
  SERIALIZE_EMPTY();
};

/// Link a new entry into a directory
class LinkVersion : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  LinkVersion(string entry, shared_ptr<Reference> target) : _entry(entry), _target(target) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Get the target of the newly-linked entry
  shared_ptr<Reference> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept override;

  /// Check to see if this version has a requested entry
  virtual optional<Resolution> getEntry(Env& env,
                                        shared_ptr<DirArtifact> dir,
                                        string name) noexcept override {
    // If the lookup is searching for the linked entry, return it.
    if (_entry == name) return _target->getArtifact();

    // No match. Return a null option so the search can continue
    return nullopt;
  }

  virtual void getKnownEntries(map<string, shared_ptr<Artifact>>& entries) noexcept override {
    entries.emplace(_entry, _target->getArtifact());
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "+" + string(_entry); }

  /// Print a link version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: link " << _entry << " -> " << _target->getName() << "]";
  }

 private:
  string _entry;
  shared_ptr<Reference> _target;

  // Create a default constructor and declare fields for serialization
  LinkVersion() = default;
  SERIALIZE(BASE(DirVersion), _entry, _target);
};

/**
 * A link directory version encodes a single linking operation, which adds an entry to the
 * directory. This is a partial version, so any attempt to resolve entries other than the linked one
 * will fall through to other versions.
 */
class UnlinkVersion : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  UnlinkVersion(string entry) : _entry(entry) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept override;

  /// Check to see if this version allows a requested entry
  virtual optional<Resolution> getEntry(Env& env,
                                        shared_ptr<DirArtifact> dir,
                                        string name) noexcept override {
    // If the lookup is searching for the unlinked entry, return ENOENT.
    if (_entry == name) return ENOENT;

    // No match, so no result from this partial version
    return nullopt;
  }

  virtual void getKnownEntries(map<string, shared_ptr<Artifact>>& entries) noexcept override {
    entries.erase(_entry);
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "-" + string(_entry); }

  /// Print an unlink version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: unlink " << _entry << "]";
  }

 private:
  string _entry;

  // Create a default constructor and declare fields for serialization
  UnlinkVersion() = default;
  SERIALIZE(BASE(DirVersion), _entry);
};

/**
 * An existing directory version is a lazily-populated set of entries that are known to be present
 * or absent. The version looks for entries using a provided environment.
 */
class ExistingDirVersion : public DirVersion {
 public:
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept override;

  /// Check if this version has a specific entry
  virtual optional<Resolution> getEntry(Env& env,
                                        shared_ptr<DirArtifact> dir,
                                        string name) noexcept override;

  virtual void getKnownEntries(map<string, shared_ptr<Artifact>>& entries) noexcept override {
    for (auto& [name, artifact] : _present) {
      entries.emplace(name, artifact);
    }
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "list"; }

  /// Print an existing directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: on-disk state]"; }

 private:
  /// Entries that are known to be in this directory
  map<string, shared_ptr<Artifact>> _present;

  /// Entries that are known NOT to be in this directory
  set<string> _absent;
};

/// A version to represent a directory that was created during the build
class EmptyDirVersion : public DirVersion {
 public:
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept override;

  /// Check if this version has a specific entry
  virtual optional<Resolution> getEntry(Env& env,
                                        shared_ptr<DirArtifact> dir,
                                        string name) noexcept override {
    return ENOENT;
  }

  virtual void getKnownEntries(map<string, shared_ptr<Artifact>>& entries) noexcept override {}

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "empty"; }

  /// Print an empty directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: empty]"; }

 private:
  // Create a default constructor and specify fields for serialization
  EmptyDirVersion() = default;
  SERIALIZE(BASE(DirVersion));
};
