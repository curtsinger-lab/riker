#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <set>

#include "runtime/Ref.hh"
#include "util/log.hh"
#include "util/serializer.hh"
#include "versions/DirListVersion.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::ostream;
using std::set;
using std::shared_ptr;

namespace fs = std::filesystem;

class Build;
class DirArtifact;
class Env;

/**
 * A DirVersion represents some part of the state of a directory. Subclasses of DirVersion can
 * encode complete versions that describe all of the contents of the directory, or partial versions
 * that describe an update to a single entry in the directory.
 */
class DirVersion : public Version {
 public:
  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept = 0;

  /// Commit this version to the filesystem
  virtual void commit(fs::path dir_path) noexcept = 0;

 private:
  SERIALIZE(BASE(Version));
};

/**
 * A BaseDirVersion encodes the starting state for a directory artifact. Every directory has exactly
 * one of these versions. Any updates to the directory are layered on top of the base version.
 */
class BaseDirVersion : public DirVersion {
 public:
  BaseDirVersion(bool created) noexcept : _created(created) { setCommitted(!created); }

  /// Does the base version represent a newly created directory?
  bool getCreated() const noexcept { return _created; }

  /// We can always commit the base version. It is either already on disk, or a newly created dir
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(fs::path dir_path) noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override {
    if (_created) {
      return "empty";
    } else {
      return "on-disk";
    }
  }

  /// Print this directory version
  virtual ostream& print(ostream& o) const noexcept override {
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

/// An AddEntry version updates a directory with a new entry
class AddEntry : public DirVersion {
 public:
  /// Create a new version of a directory that adds a named entry to the directory
  AddEntry(string entry, shared_ptr<Artifact> target) noexcept : _entry(entry), _target(target) {}

  /// Get the name of the entry this version links
  string getEntryName() const noexcept { return _entry; }

  /// Get the target of the newly-linked entry
  shared_ptr<Artifact> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override {
    // We can always commit a link to an artifact: it either has a path we can hard link to, or we
    // could create it. We will always be able to commit the artifact because created links depend
    // on the current artifact state.
    return true;
  }

  /// Commit this version to the filesystem
  virtual void commit(fs::path dir_path) noexcept override;

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "+" + string(_entry); }

  /// Print a link version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[dir: link " << _entry << " -> " << _target << "]";
  }

 private:
  string _entry;
  shared_ptr<Artifact> _target;
};

/// A RemoveEntry version updates a directory so it no longer has a specific entry
class RemoveEntry : public DirVersion {
 public:
  /// Create a new version of a directory that removes an entry from a directory
  RemoveEntry(string entry, shared_ptr<Artifact> target) noexcept :
      _entry(entry), _target(target) {}

  /// Get the name of the entry this version removes
  string getEntryName() const noexcept { return _entry; }

  /// Get a reference to the artifact that is unlinked
  shared_ptr<Artifact> getTarget() const noexcept { return _target; }

  /// Can this version be committed to the filesystem?
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit this version to the filesystem
  virtual void commit(fs::path dir_path) noexcept override;

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
  shared_ptr<Artifact> _target;
};
