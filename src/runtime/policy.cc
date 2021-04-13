#include "policy.hh"

#include <filesystem>
#include <memory>
#include <set>
#include <sstream>
#include <string>

#include "util/log.hh"
#include "util/options.hh"
#include "versions/ContentVersion.hh"

using std::set;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

class Command;

namespace policy {

  set<string> never_cache = {"/dev/null"};

  /// Check if the given path has the current working directory as a prefix
  static bool localPath(fs::path path) {
    // Get the current working directory
    static fs::path cwd = fs::current_path();

    // We'll walk through the cwd and candidate path together
    auto cwd_iter = cwd.begin();
    auto path_iter = path.begin();

    // Loop as long as both paths have parts
    while (cwd_iter != cwd.end() && path_iter != path.end()) {
      // If the paths differ at this point, the path is not local
      if (*path_iter != *cwd_iter) return false;

      // Advance both iterators
      cwd_iter++;
      path_iter++;
    }

    // The candidate path started with the current working directory, so return true
    return true;
  }

  FingerprintType chooseFingerprintType(const shared_ptr<Command>& reader,
                                        const shared_ptr<Command>& writer,
                                        fs::path path) {
    // If the fingerprinting policy is None, always collect quick fingerprints
    if (options::fingerprint_level == FingerprintLevel::None) {
      LOG(cache) << "Selected quick fingerprint for path " << path
                 << " because fingerprint level is set to None.";
      return FingerprintType::Quick;
    }

    // If the fingerprinting policy is All, always collect full fingerprints
    if (options::fingerprint_level == FingerprintLevel::All) {
      LOG(cache) << "Selected full fingerprint for path " << path
                 << " because fingerprint level is set to All.";

      return FingerprintType::Full;
    }

    // If we've made it to this point, only local and intermediate versions are fingerprinted

    // Does the version have both a reader and a creator?
    if (writer && reader) {
      // Yes. If the reader and creator are the same command, we don't collect a fingerprint
      // Otherwise take a full fingerprint
      if (writer == reader) {
        LOG(cache) << "Selected no fingerprint for path " << path
                   << " because the version is read by its writer.";

        return FingerprintType::None;

      } else {
        LOG(cache) << "Selected full fingerprint for path " << path
                   << " because it is an intermediate file.";

        return FingerprintType::Full;
      }

    } else {
      // Missing a reader or creator. We only fingerprint the file if its path is local
      if (localPath(path)) {
        LOG(cache) << "Selected full fingerprint for path " << path << " because path is local.";
        return FingerprintType::Full;

      } else {
        LOG(cache) << "Selected quick fingerprint for path " << path
                   << " because path is not local.";
        return FingerprintType::Quick;
      }
    }
  }

  bool isCacheable(const shared_ptr<Command>& reader,
                   const shared_ptr<Command>& writer,
                   fs::path path) {
    // is this a special file?
    if (never_cache.find(path.string()) != never_cache.end()) {
      LOG(cache) << "Policy: versions for file " << path << " are never cached.";
      return false;
    }

    bool is_fingerprintable = chooseFingerprintType(reader, writer, path) != FingerprintType::None;
    bool has_creator = writer != nullptr;
    bool cache_enabled = options::enable_cache;

    // we cache if v is fingerprintable, was created by a build command, and caching is enabled
    bool do_cache = is_fingerprintable && writer && options::enable_cache;

    LOG(cache) << "Policy: for path " << path << (do_cache ? " should" : " should not")
               << " be cached because it"
               << (is_fingerprintable ? " is fingerprintable," : " is not fingerprintable,")
               << (has_creator ? " has a creator," : " has no creator,") << " and"
               << (cache_enabled ? " cache is enabled" : " cache is disabled") << ".";

    return do_cache;
  }
}
