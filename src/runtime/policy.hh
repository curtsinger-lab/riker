#pragma once

#include <filesystem>
#include <memory>
#include <optional>

using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class Command;
class ContentVersion;

/// Returns true iff the version is fingerprintable
bool isFingerprintable(const shared_ptr<Command>& c,
                       const optional<fs::path>& p,
                       const shared_ptr<ContentVersion>& v);

/// Returns true iff the version is cachable
bool isCachable(const shared_ptr<Command>& c,
                const optional<fs::path>& p,
                const shared_ptr<ContentVersion>& v);