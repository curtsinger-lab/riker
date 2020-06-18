#pragma once

#include <filesystem>
#include <memory>
#include <set>

#include "util/log.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::set;
using std::shared_ptr;

namespace fs = std::filesystem;

enum class Lookup { Yes, No, Maybe };

class DirVersion : public Version {
 public:
  virtual Lookup hasEntry(fs::path name) const noexcept = 0;

 private:
  SERIALIZE_EMPTY();
};

class ListedDirVersion : public DirVersion {
 public:
  /// Create a ListedDirVersion for an initially-empty directory
  ListedDirVersion() {
    // Add placeholder entries for "." and ".."
    _entries.emplace(".");
    _entries.emplace("..");
  }

  /// Create a ListedDirVersion by listing an on-disk directory
  ListedDirVersion(fs::path dirpath) : ListedDirVersion() {
    // Add entries from an on-disk directory.
    // The directory_iterator interface excludes . and .., so run the default constructor too.
    for (auto entry : fs::directory_iterator(dirpath)) {
      _entries.emplace(entry.path().filename());
    }
  }

  /// Get the name for this version type
  virtual string getTypeName() const noexcept override { return "dir list"; }

  /// Directory lists are never saved (at least for now)
  virtual bool isSaved() const noexcept override { return false; }

  /// Do nothing when asked to save the directory
  virtual void save(shared_ptr<Reference> ref) noexcept override {}

  /// Do nothing when asked to commit the directory
  virtual void commit(shared_ptr<Reference> ref) const noexcept override {}

  /// Directory lists are always fingerprinted
  virtual bool hasFingerprint() const noexcept override { return true; }

  /// Do nothing when asked to fingerprint the directory
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept override {}

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    WARN << "Unimplemented directory version comparion!";
    return false;
  }

  /// Check if this version has a specific entry
  virtual Lookup hasEntry(fs::path name) const noexcept override {
    return _entries.find(name) == _entries.end() ? Lookup::No : Lookup::Yes;
  }

 private:
  set<fs::path> _entries;

  // Specify fields for serialization
  SERIALIZE(BASE(DirVersion), _entries);
};
