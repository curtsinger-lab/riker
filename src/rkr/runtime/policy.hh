#pragma once

#include <filesystem>
#include <memory>

#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

class Command;

namespace policy {
  /// Get the appropriate fingerprint type for a content version
  FingerprintType chooseFingerprintType(const std::shared_ptr<Command>& reader,
                                        const std::shared_ptr<Command>& writer,
                                        fs::path path);

  /// Returns true iff the version is cachable
  bool isCacheable(const std::shared_ptr<Command>& reader,
                   const std::shared_ptr<Command>& writer,
                   fs::path path);
}