#pragma once

#include <filesystem>
#include <memory>

#include "versions/ContentVersion.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

class Command;

namespace policy {
  /// Get the appropriate fingerprint type for a content version
  FingerprintType chooseFingerprintType(const shared_ptr<Command>& reader,
                                        fs::path path,
                                        const shared_ptr<ContentVersion>& version);

  /// Returns true iff the version is cachable
  bool isCacheable(const shared_ptr<Command>& reader,
                   fs::path path,
                   const shared_ptr<ContentVersion>& version);
}