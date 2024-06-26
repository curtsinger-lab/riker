#pragma once

#include <filesystem>
#include <memory>
#include <ostream>
#include <set>
#include <string>

#include "util/log.hh"
#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

/**
 * A DirListVersion stores a list of all entries in a directory. This version is created
 * on-demand when a command lists the contents of a directory. These versions can be matched against
 * a directory, but are never used to update the contents of a directory.
 */
class DirListVersion : public ContentVersion {
 public:
  DirListVersion() noexcept = default;

  /// Check if this list matches another list
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override {
    auto other_list = other->as<DirListVersion>();
    if (!other_list) return false;
    return _entries == other_list->_entries;
  }

  /// Get the name for the type of version this is
  virtual std::string getTypeName() const noexcept override { return "dir list"; }

  /// Print this version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    o << "[dir: {";
    bool first = true;
    for (const auto& entry : _entries) {
      if (!first) o << ", ";
      first = false;
      o << entry;
    }
    return o << "}]";
  }

  /// Add an entry to this listed directory version
  void addEntry(fs::path name) noexcept { _entries.insert(name); }

  /// Remove an entry from this listed directory version
  void removeEntry(fs::path name) noexcept { _entries.erase(name); }

  /// Get the entries in this version
  const std::set<fs::path>& getEntries() const noexcept { return _entries; }

 private:
  /// The names of entries in the directory
  std::set<fs::path> _entries;
};