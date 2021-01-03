#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "versions/Version.hh"

static bool isFingerprintablePredicate(shared_ptr<Command> c,
                                       shared_ptr<std::optional<fs::path>> p,
                                       shared_ptr<Version> v) {
  // sometimes we want to fingerprint things at startup, when there is no running command
  if (!c) return p->has_value();

  // otherwise, get the creator of the given version
  auto creator = v->getCreator();

  // and fingerprint if v was not created by c and v has a path
  return creator != c->currentRun() && p->has_value();
}

bool isFingerprintable(shared_ptr<Command> c,
                       shared_ptr<std::optional<fs::path>> p,
                       shared_ptr<Version> v) {
  bool do_fingerprint = isFingerprintablePredicate(c, p, v);

  if (!c) {
    LOG(cache) << "Policy: version " << v << " for path "
               << (p->has_value() ? p->value() : "(no path)")
               << (do_fingerprint ? " should" : " should not") << " be fingerprinted because it"
               << (p->has_value() ? " has a path" : " does not have a path") << ".";
  } else {
    LOG(cache) << "Policy: version " << v << " for path "
               << (p->has_value() ? p->value() : "(no path)")
               << (do_fingerprint ? " should" : " should not") << " be fingerprinted because it"
               << (c ? " was created by the build," : " was not created by the build,")
               << (v->getCreator() != c->currentRun() ? " has a different creating command,"
                                                      : " was created by this command,")
               << " and" << (p->has_value() ? " has a path" : " does not have a path") << ".";
  }

  return do_fingerprint;
}

bool isCachable(shared_ptr<Command> c,
                shared_ptr<std::optional<fs::path>> p,
                shared_ptr<Version> v) {
  // get the creator of the given version
  auto creator = v->getCreator();

  bool is_fingerprintable = isFingerprintablePredicate(c, p, v);
  bool has_creator = creator != nullptr;
  bool cache_enabled = options::enable_cache;

  // we cache if v is fingerprintable, was created by a build command, and caching is enabled
  bool do_cache =
      isFingerprintablePredicate(c, p, v) && creator != nullptr && options::enable_cache;

  LOG(cache) << "Policy: version " << v << " for path "
             << (p->has_value() ? p->value() : "(no path)")
             << (do_cache ? " should" : " should not") << " be cached because it"
             << (is_fingerprintable ? " is fingerprintable," : " is not fingerprintable,")
             << (has_creator ? " has a creator," : " has no creator,") << " and"
             << (cache_enabled ? " cache is enabled" : " cache is disabled") << ".";

  return do_cache;
}