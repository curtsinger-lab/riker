#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <set>

#include "core/IR.hh"
#include "util/log.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::ostream;
using std::set;
using std::shared_ptr;

namespace fs = std::filesystem;

class Env;

/// Possible returned values from an attempt to get an entry from a directory version
enum class Lookup { Yes, No, Maybe };

/// Base class for all of the various types of directory versions
class DirVersion : public Version {
 public:
  /**
   * Check to see if this directory version guarantees the presence or absence of a named entry.
   * A yes or no answer is definite, but partial versions can return "maybe", indicating that
   * checking should continue on to additional version.
   */
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept = 0;

  /**
   * Get the artifact corresponding to a named entry.
   * Returning nullptr indicates that the directory should get the artifact from the filesystem.
   */
  virtual shared_ptr<Artifact> getEntry(string name) const noexcept = 0;

 private:
  SERIALIZE_EMPTY();
};

/// Link a new entry into a directory
class LinkVersion : public DirVersion {
 public:
  /// Default constructor is required for Build::apply, but should only be used for deserialization
  LinkVersion() = default;

  /// Create a new version of a directory that adds a named entry to the directory
  LinkVersion(string entry, shared_ptr<Reference> target) : _entry(entry), _target(target) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Get the target of the newly-linked entry
  shared_ptr<Reference> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> dir_ref) noexcept override;

  /// Check to see if this version has a requested entry
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override {
    // If the lookup is searching for the linked entry, return yes. Otherwise fall through.
    if (_entry == name) return Lookup::Yes;
    return Lookup::Maybe;
  }

  /// Get the artifact this linked entry refers to
  virtual shared_ptr<Artifact> getEntry(string name) const noexcept override {
    ASSERT(name == _entry) << "Requested invalid entry from LinkVersion";
    return _target->getArtifact();
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

  // Declare fields for serialization
  SERIALIZE(BASE(DirVersion), _entry, _target);
};

/**
 * A link directory version encodes a single linking operation, which adds an entry to the
 * directory. This is a partial version, so any attempt to resolve entries other than the linked one
 * will fall through to other versions.
 */
class UnlinkVersion : public DirVersion {
 public:
  /// Default constructor is required for Build::apply, but should only be used for deserialization
  UnlinkVersion() = default;

  /// Create a new version of a directory that adds a named entry to the directory
  UnlinkVersion(string entry) : _entry(entry) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> dir_ref) noexcept override;

  /// Check to see if this version allows a requested entry
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override {
    // If the lookup is searching for the unlinked entry, return "no". Otherwise return "maybe".
    if (_entry == name) return Lookup::No;
    return Lookup::Maybe;
  }

  /// Get an artifact from this entry. Should never be called.
  virtual shared_ptr<Artifact> getEntry(string name) const noexcept override {
    ASSERT(false) << "Requested entry from UnlinkVersion";
    return nullptr;
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "-" + string(_entry); }

  /// Print an unlink version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: unlink " << _entry << "]";
  }

 private:
  string _entry;

  // Declare fields for serialization
  SERIALIZE(BASE(DirVersion), _entry);
};

/**
 * An existing directory version is a lazily-populated set of entries that are known to be present
 * or absent. The version looks for entries using a provided environment.
 */
class ExistingDirVersion : public DirVersion {
 public:
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return false; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> dir_ref) noexcept override;

  /// Check if this version has a specific entry
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override;

  /// Get a specific entry from this version
  virtual shared_ptr<Artifact> getEntry(string name) const noexcept override { return nullptr; }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "list"; }

  /// Print an existing directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: on-disk state]"; }

 private:
  /// Entries that are known to be in this directory
  set<string> _present;

  /// Entries that are known NOT to be in this directory
  set<string> _absent;

  // Declare fields for serialization
  SERIALIZE(BASE(DirVersion), _present, _absent);
};

/// A version to represent a directory that was created during the build
class EmptyDirVersion : public DirVersion {
 public:
  /// Create an EmptyDirVersion
  EmptyDirVersion() = default;

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> dir_ref) noexcept override;

  /// Check if this version has a specific entry
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override {
    if (name == "." || name == "..") return Lookup::Yes;
    return Lookup::No;
  }

  /// Get a specific entry from this version
  virtual shared_ptr<Artifact> getEntry(string name) const noexcept override { return nullptr; }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "empty"; }

  /// Print an empty directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: empty]"; }

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(DirVersion));
};
