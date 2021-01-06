#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "ui/options.hh"
#include "versions/ContentVersion.hh"

set<string> never_cache = {"/dev/null"};

static bool fingerprintPath(fs::path path) {
  // If all files are fingerprinted, return true immediately
  if (options::fingerprint_level == FingerprintLevel::All) return true;

  // If no files are fingerprinted, return false immediately
  if (options::fingerprint_level == FingerprintLevel::None) return false;

  // Otherwise, only local files are fingerprinted

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

static bool isFingerprintablePredicate(const shared_ptr<Command>& c,
                                       const optional<fs::path>& p,
                                       const shared_ptr<ContentVersion>& v) {
  // If there is no path, do not fingerprint
  if (!p.has_value()) return false;

  // If there is no reader, fingerprint files with an acceptable path
  if (!c) return fingerprintPath(p.value());

  // If the version was not created by a command, fingerprint it only if it has an acceptable path
  if (!v->getCreator()) return fingerprintPath(p.value());

  // Finally, if the version was created by a command, fingerprint it if that command is not reading
  return v->getCreator() != c;
}

bool isFingerprintable(const shared_ptr<Command>& c,
                       const optional<fs::path>& p,
                       const shared_ptr<ContentVersion>& v) {
  bool do_fingerprint = isFingerprintablePredicate(c, p, v);

  if (!c) {
    LOG(cache) << "Policy: version " << v << " for path "
               << (p.has_value() ? p.value() : "(no path)")
               << (do_fingerprint ? " should" : " should not") << " be fingerprinted because it"
               << (p.has_value() ? " has a path" : " does not have a path") << ".";
  } else {
    LOG(cache) << "Policy: version " << v << " for path "
               << (p.has_value() ? p.value() : "(no path)")
               << (do_fingerprint ? " should" : " should not") << " be fingerprinted because it"
               << (c ? " was created by the build," : " was not created by the build,")
               << (v->getCreator() != c ? " has a different creating command,"
                                        : " was created by this command,")
               << " and" << (p.has_value() ? " has a path" : " does not have a path") << ".";
  }

  return do_fingerprint;
}

bool isCachable(const shared_ptr<Command>& c,
                const optional<fs::path>& p,
                const shared_ptr<ContentVersion>& v) {
  // is this a special file?
  if (p.has_value()) {
    auto path = p.value().string();
    if (never_cache.find(path) != never_cache.end()) {
      LOG(cache) << "Policy: versions for file " << path << " are never cached.";
      return false;
    }
  }

  // get the creator of the given version
  auto creator = v->getCreator();

  bool is_fingerprintable = isFingerprintablePredicate(c, p, v);
  bool has_creator = creator != nullptr;
  bool cache_enabled = options::enable_cache;

  // we cache if v is fingerprintable, was created by a build command, and caching is enabled
  bool do_cache =
      isFingerprintablePredicate(c, p, v) && creator != nullptr && options::enable_cache;

  LOG(cache) << "Policy: version " << v << " for path " << (p.has_value() ? p.value() : "(no path)")
             << (do_cache ? " should" : " should not") << " be cached because it"
             << (is_fingerprintable ? " is fingerprintable," : " is not fingerprintable,")
             << (has_creator ? " has a creator," : " has no creator,") << " and"
             << (cache_enabled ? " cache is enabled" : " cache is disabled") << ".";

  return do_cache;
}