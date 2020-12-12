#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <set>
#include <string>

#include "util/log.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

/**
 * A DirListVersion stores a list of all entries in a directory. This version is created
 * on-demand when a command lists the contents of a directory. These versions can be matched against
 * a directory, but are never used to update the contents of a directory.
 */
class DirListVersion : public Version {
 public:
  DirListVersion() noexcept = default;

  /// Check if this list matches another list
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    auto other_list = other->as<DirListVersion>();
    if (!other_list) return false;
    return _entries == other_list->_entries;
  }

  /// Get the name for the type of version this is
  virtual string getTypeName() const noexcept override { return "listed"; }

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[dir: listed]"; }

  /// Add an entry to this listed directory version
  void addEntry(fs::path entry) noexcept { _entries.insert(entry); }

  /// Remove an entry from this listed directory version
  void removeEntry(fs::path entry) noexcept { _entries.erase(entry); }

 private:
  /// The names of entries in the directory
  set<fs::path> _entries;

  SERIALIZE(BASE(Version), _entries);
};