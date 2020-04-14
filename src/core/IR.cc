#include "IR.hh"

#include <map>
#include <ostream>
#include <utility>

#include "core/Command.hh"

using std::ostream;

// Check the result of a Reference::Access
int Reference::Access::checkAccess() {
  // Set up an access mode that we'll check
  int access_mode = 0;
  if (_flags.r) access_mode |= R_OK;
  if (_flags.w) access_mode |= W_OK;
  if (_flags.x) access_mode |= X_OK;

  // TODO: Add support for the create flag: if the access fails with ENOENT, but writing to the
  // directory would succeed, we can return success.

  // TODO: Add support for the exclusive flag: if create and exclusive are specified, we have to
  // make sure the file does NOT exist, and the user can write to the containing directory.

  // TODO: Is there anything to do for truncate? We need to be sure we can write the file, but is
  // it even possible to open with O_TRUNC in read-only mode?

  // Normally, faccessat checks whether the real user has access. We want to check as whatever the
  // effective user is. That's the same permission level the build would run with.
  int access_flags = AT_EACCESS;

  // Check access on a symlink if nofollow is specified
  if (_flags.nofollow) access_flags |= AT_SYMLINK_NOFOLLOW;

  // Use faccessat to check the reference
  if (faccessat(AT_FDCWD, _path.c_str(), access_mode, access_flags)) {
    // If there's an error, return the error value stored in errno
    return errno;
  } else {
    // If not, return success
    return 0;
  }
}

// Check if a reference would resolve the same way on rebuild
bool Predicate::ReferenceResult::eval(map<string, ArtifactVersion>& env) {
  int result = _ref->checkAccess();
  if (result != _rc) {
    LOG << "Reference returned " << result << " instead of " << _rc;
    return false;
  } else {
    return true;
  }
}

// Check if a MetadataMatch predicate would resolve the same way on rebuild
bool Predicate::MetadataMatch::eval(map<string, ArtifactVersion>& env) {
  optional<string> path = _ref->getPath();

  // References without paths never check out
  if (!path.has_value()) return false;

  // If the environment has this path, we can check the version cached there
  auto iter = env.find(path.value());
  if (iter != env.end()) {
    // This is probably overly-conservative. Any version with the same metadata would be okay.
    return iter->second == _version;
  } else {
    // Check the contents of the referred-to path
    // TODO: handle nofollow flag
    return _version.metadataMatch(path.value());
  }
}

// Check if a ContentsMatch predicate would resolve the same ay on rebuild
bool Predicate::ContentsMatch::eval(map<string, ArtifactVersion>& env) {
  optional<string> path = _ref->getPath();

  // References without paths never check out
  if (!path.has_value()) return false;

  // If the environment has this path, we can check the version cached there
  auto iter = env.find(path.value());
  if (iter != env.end()) {
    // This is overly-conservative. Any version with the same contents would be okay.
    return iter->second == _version;
  } else {
    // Check the contents of the referred-to path
    // TODO: handle nofollow flag
    return _version.contentsMatch(path.value());
  }
}

// Run a SetMetadata action
bool Action::SetMetadata::eval(map<string, ArtifactVersion>& env) {
  optional<string> path = _ref->getPath();

  // If the referred-to artifact doesn't have a path, there's nothing left to do
  if (!path.has_value()) return true;

  // We have a path. Record the effect of this action in the environment
  env[path.value()] = _version;

  // Evaluation succeeds
  return true;
}

// Run a SetContents action
bool Action::SetContents::eval(map<string, ArtifactVersion>& env) {
  optional<string> path = _ref->getPath();

  // If the referred-to artifact doesn't have a path, there's nothing left to do
  if (!path.has_value()) return true;

  // We have a path. Record the effect of this action in the environment
  env[path.value()] = _version;

  // Evaluation succeeds
  return true;
}

// Print a ReferenceResult predicate
ostream& Predicate::ReferenceResult::print(ostream& o) const {
  // Set up a map from error codes to names
  static map<int, string> errors = {{0, "SUCCESS"},     {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
                                    {EEXIST, "EEXIST"}, {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
                                    {ELOOP, "ELOOP"},   {ENOENT, "ENOENT"}};

  // If we can't identify the error code, just print "EMYSTERY"
  string errname = "EMYSTERY";

  // Look up the error name in our map
  auto iter = errors.find(_rc);
  if (iter != errors.end()) {
    errname = iter->second;
  }

  return o << "REFERENCE_RESULT(" << _ref->getName() << ", " << errname << ")";
}

// Print a launch action
ostream& Action::Launch::print(ostream& o) const {
  o << "LAUNCH(" << _cmd << ", [";
  bool first = true;
  for (auto& entry : _cmd->getInitialFDs()) {
    if (!first) o << ", ";
    first = false;
    o << entry.second.getReference()->getName();
  }
  return o << "])";
}
