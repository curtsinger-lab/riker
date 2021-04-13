#pragma once

#include <filesystem>

namespace fs = std::filesystem;

// Namespace to contain global flags that control build behavior
namespace constants {
  /// What is the name of the build state directory?
  const fs::path OutputDir = ".rkr";

  /// What is the name of the build database?
  const fs::path DatabaseFilename = OutputDir / "db";

  /// What is the name of the new build database?
  const fs::path NewDatabaseFilename = OutputDir / "newdb";

  /// Where are cached files saved?
  const fs::path CacheDir = OutputDir / "cache";

  /// Where are cached files saved?
  const fs::path NewCacheDir = OutputDir / "newcache";
}