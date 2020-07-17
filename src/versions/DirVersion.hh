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
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept = 0;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept = 0;

 private:
  SERIALIZE_EMPTY();
};

/// Every directory has a base version that serves as the last stop when checking for entries
class BaseDirVersion : public DirVersion {
 public:
  /// Check for a named entry in this directory version
  virtual Resolution getEntry(Env& env, shared_ptr<DirArtifact> dir, string name) noexcept = 0;

 private:
  SERIALIZE(BASE(DirVersion));
};

/// A version to represent a directory that was created during the build
class CreatedDir : public BaseDirVersion {
 public:
  /// Create a new empty directory version
  CreatedDir() noexcept = default;

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept override;

  /// Check if this version has a specific entry
  virtual Resolution getEntry(Env& env,
                              shared_ptr<DirArtifact> dir,
                              string name) noexcept override {
    return ENOENT;
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "empty"; }

  /// Print an empty directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: empty]"; }

 private:
  // Specify fields for serialization
  SERIALIZE(BASE(BaseDirVersion));
};

/// An existing directory version checks for entries against the directory on the filesystem
class ExistingDir : public BaseDirVersion {
 public:
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept override {
    FAIL_IF(!isCommitted()) << "An existing directory " << dir << " was in an uncommitted state";
  }

  /// Check if this version has a specific entry
  virtual Resolution getEntry(Env& env, shared_ptr<DirArtifact> dir, string name) noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "on-disk"; }

  /// Print an existing directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: on-disk state]"; }
};

/// Link a new entry into a directory
class AddEntry : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  AddEntry(string entry, shared_ptr<Ref> target) : _entry(entry), _target(target) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Get the target of the newly-linked entry
  shared_ptr<Ref> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override {
    // We can always commit a link to an artifact: it either has a path we can hard link to, or we
    // could create it. We will always be able to commit the artifact because created links depend
    // on the current artifact state.
    return true;
  }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "+" + string(_entry); }

  /// Print a link version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: link " << _entry << " -> " << _target->getName() << "]";
  }

 private:
  string _entry;
  shared_ptr<Ref> _target;

  // Create a default constructor and declare fields for serialization
  AddEntry() = default;
  SERIALIZE(BASE(DirVersion), _entry, _target);
};

/**
 * A link directory version encodes a single linking operation, which adds an entry to the
 * directory. This is a partial version, so any attempt to resolve entries other than the linked one
 * will fall through to other versions.
 */
class RemoveEntry : public DirVersion {
 public:
  /// Create a new version of a directory that removes an entry from a directory
  RemoveEntry(string entry, shared_ptr<Ref> target) : _entry(entry), _target(target) {}

  /// Get the name of the entry this version removes
  string getEntryName() const noexcept { return _entry; }

  /// Get a reference to the artifact that is unlinked
  shared_ptr<Ref> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "-" + string(_entry); }

  /// Print an unlink version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: unlink " << _entry << "]";
  }

 private:
  /// The name of the entry this version removes
  string _entry;

  /// A reference to the artifact that is unlinked by this version
  shared_ptr<Ref> _target;

  // Create a default constructor and declare fields for serialization
  RemoveEntry() = default;
  SERIALIZE(BASE(DirVersion), _entry, _target);
};
