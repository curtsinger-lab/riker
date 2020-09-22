#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <set>

#include "build/Resolution.hh"
#include "core/RefResult.hh"
#include "util/log.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::ostream;
using std::set;
using std::shared_ptr;

namespace fs = std::filesystem;

class Build;
class DirArtifact;
class Env;

/// A ListedDir version stores a list of all entries in a directory. It does not need to be a
/// DirVersion because it cannot be committed or applied, only matched to a directory.
class ListedDir : public Version {
 public:
  ListedDir() noexcept = default;

  /// Save a fingerprint for this version, which is always done on creation
  virtual void fingerprint(fs::path path) noexcept override {}

  /// Check if this version has a fingerprint
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Check if this list matches another list
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    auto other_list = other->as<ListedDir>();
    if (!other_list) return false;
    return _entries == other_list->_entries;
  }

  /// Get the name for the type of version this is
  virtual string getTypeName() const noexcept override { return "listed"; }

  /// Apply this version to an artifact
  virtual void applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept override {
    FAIL << "Cannot apply a listed directory version";
  }

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: listed]"; }

 protected:
  friend class DirArtifact;
  friend class ExistingDir;

  /// Add an entry to this listed directory version
  void addEntry(string name) noexcept { _entries.insert(name); }

  /// Remove an entry from this listed directory version
  void removeEntry(string name) noexcept { _entries.erase(name); }

 private:
  set<string> _entries;

  SERIALIZE(BASE(Version), _entries);
};

/// Base class for all of the various types of directory versions
class DirVersion : public Version {
 public:
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept = 0;

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept = 0;

  /// Skip fingerprints for partial directory versions
  virtual void fingerprint(fs::path path) noexcept override {}

  /// Check if this version has a fingerprint
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Disallow direct comparisons of partial directory versions
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    FAIL << "Attempted to match against a partial directory version";
    return false;
  }

 private:
  SERIALIZE(BASE(Version));
};

/// Every directory has a base version that serves as the last stop when checking for entries
class BaseDirVersion : public DirVersion {
 public:
  /// Check for a named entry in this directory version
  virtual Resolution getEntry(Build& build,
                              shared_ptr<Env> env,
                              shared_ptr<DirArtifact> dir,
                              string name) noexcept = 0;

  /// Create a listed directory version from this base directory
  virtual shared_ptr<ListedDir> getList(shared_ptr<Env> env,
                                        shared_ptr<DirArtifact> dir) const noexcept = 0;

  /// Apply this version to an artifact
  virtual void applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept override {
    FAIL << "Cannot apply a base directory version";
  }

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
  virtual Resolution getEntry(Build& build,
                              shared_ptr<Env> env,
                              shared_ptr<DirArtifact> dir,
                              string name) noexcept override {
    return ENOENT;
  }

  /// Create a listed directory version from this base directory
  virtual shared_ptr<ListedDir> getList(shared_ptr<Env> env,
                                        shared_ptr<DirArtifact> dir) const noexcept override {
    return make_shared<ListedDir>();
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
  ExistingDir() noexcept = default;

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept override {
    FAIL_IF(!isCommitted()) << "An existing directory " << dir << " was in an uncommitted state";
  }

  /// Check if this version has a specific entry
  virtual Resolution getEntry(Build& build,
                              shared_ptr<Env> env,
                              shared_ptr<DirArtifact> dir,
                              string name) noexcept override;

  /// Create a listed directory version from this base directory
  virtual shared_ptr<ListedDir> getList(shared_ptr<Env> env,
                                        shared_ptr<DirArtifact> dir) const noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "on-disk"; }

  /// Print an existing directory version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: on-disk state]"; }
};

/// An AddEntry version updates a directory with a new entry
class AddEntry : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  AddEntry(string entry, shared_ptr<RefResult> target) noexcept : _entry(entry), _target(target) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Get the target of the newly-linked entry
  shared_ptr<RefResult> getTarget() const noexcept { return _target; }

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

  /// Apply this version to an artifact
  virtual void applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept override;

  /// Print a link version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: link " << _entry << " -> " << _target->getName() << "]";
  }

 private:
  string _entry;
  shared_ptr<RefResult> _target;
};

/// A RemoveEntry version updates a directory so it no longer has a specific entry
class RemoveEntry : public DirVersion {
 public:
  /// Create a new version of a directory that removes an entry from a directory
  RemoveEntry(string entry, shared_ptr<RefResult> target) noexcept :
      _entry(entry), _target(target) {}

  /// Get the name of the entry this version removes
  string getEntryName() const noexcept { return _entry; }

  /// Get a reference to the artifact that is unlinked
  shared_ptr<RefResult> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "-" + string(_entry); }

  /// Apply this version to an artifact
  virtual void applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept override;

  /// Print an unlink version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: unlink " << _entry << "]";
  }

 private:
  /// The name of the entry this version removes
  string _entry;

  /// A reference to the artifact that is unlinked by this version
  shared_ptr<RefResult> _target;
};
