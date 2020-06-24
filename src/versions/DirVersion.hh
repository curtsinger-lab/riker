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

/**
 * A directory version encodes some or all of the state of a directory. Unlike versions for other
 * artifact types, directory versions can be *partial*. These partial versions encode specific
 * actions like linking or unlinking an entry in a directory.
 */
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
  virtual shared_ptr<Artifact> getEntry(string name) const noexcept { return nullptr; }

 private:
  SERIALIZE_EMPTY();
};

/**
 * A link directory version encodes a single linking operation, which adds an entry to the
 * directory. This is a partial version, so any attempt to resolve entries other than the linked one
 * will fall through to other versions.
 */
class LinkDirVersion : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  LinkDirVersion(string entry, shared_ptr<Reference> target) : _entry(entry), _target(target) {}

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "+" + string(_entry); }

  /// Directory links are never saved (at least for now)
  virtual bool isSaved() const noexcept override { return false; }

  /// Directory links are always fingerprinted
  virtual bool hasFingerprint() const noexcept override { return true; }

  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override {
    // If the lookup is searching for the linked entry, return yes. Otherwise fall through.
    if (_entry == name) return Lookup::Yes;
    return Lookup::Maybe;
  }

  virtual shared_ptr<Artifact> getEntry(string name) const noexcept override {
    // If the lookup is searching for the linke entry, return the corresponding artifact.
    if (name == _entry) return _target->getArtifact();

    return nullptr;
  }

  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: link " << _entry << " -> " << _target->getName() << "]";
  }

 private:
  string _entry;
  shared_ptr<Reference> _target;

  // Create default constructor and declare fields for serialization
  LinkDirVersion() = default;
  SERIALIZE(BASE(DirVersion), _entry, _target);
};

/**
 * A link directory version encodes a single linking operation, which adds an entry to the
 * directory. This is a partial version, so any attempt to resolve entries other than the linked one
 * will fall through to other versions.
 */
class UnlinkDirVersion : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  UnlinkDirVersion(string entry) : _entry(entry) {}

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "-" + string(_entry); }

  /// Directory links are never saved (at least for now)
  virtual bool isSaved() const noexcept override { return false; }

  /// Directory links are always fingerprinted
  virtual bool hasFingerprint() const noexcept override { return true; }

  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override {
    // If the lookup is searching for the linked entry, return yes. Otherwise fall through.
    if (_entry == name) return Lookup::No;
    return Lookup::Maybe;
  }

  virtual shared_ptr<Artifact> getEntry(string name) const noexcept override { return nullptr; }

  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: unlink " << _entry << "]";
  }

 private:
  string _entry;

  // Create default constructor and declare fields for serialization
  UnlinkDirVersion() = default;
  SERIALIZE(BASE(DirVersion), _entry);
};

/**
 * An existing directory version is a lazily-populated set of entries that are known to be present
 * or absent. The version looks for entries using a provided environment.
 */
class ExistingDirVersion : public DirVersion {
 public:
  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "list"; }

  /// Directory lists are never saved (at least for now)
  virtual bool isSaved() const noexcept override { return false; }

  /// Directory lists are always fingerprinted
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Check if this version has a specific entry
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override;

  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: on-disk state]"; }

 private:
  /// Entries that are known to be in this directory
  set<string> _present;

  /// Entries that are known NOT to be in this directory
  set<string> _absent;

  // Declare fields for serialization
  SERIALIZE(BASE(DirVersion), _present, _absent);
};

/**
 * A listed directory version is a complete list of the entries in a directory. This can appear as
 * the initial version for a directory that is created during a build. These versions can also be
 * created on-demand when a command lists a directory that has a number of partial versions.
 */
class ListedDirVersion : public DirVersion {
 public:
  /// Create a ListedDirVersion for an initially-empty directory
  ListedDirVersion() {
    // Add placeholder entries for "." and ".."
    _entries.emplace(".");
    _entries.emplace("..");
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "list"; }

  /// Directory lists are never saved (at least for now)
  virtual bool isSaved() const noexcept override { return false; }

  /// Directory lists are always fingerprinted
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Check if this version has a specific entry
  virtual Lookup hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept override {
    return _entries.find(name) == _entries.end() ? Lookup::No : Lookup::Yes;
  }

  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: empty]"; }

 private:
  set<string> _entries;

  // Specify fields for serialization
  SERIALIZE(BASE(DirVersion), _entries);
};
