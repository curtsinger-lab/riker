#include "IR.hh"

#include <map>
#include <ostream>
#include <utility>

#include "core/Command.hh"

using std::ostream;

// Set up a map from return codes to names
static map<int, string> errors = {{SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
                                  {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
                                  {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}};

// Check the result of a Reference::Access
int Reference::Access::checkAccess() {
  // Set up an access mode that we'll check
  int access_mode = 0;
  if (_flags.r) access_mode |= R_OK;
  if (_flags.w) access_mode |= W_OK;
  if (_flags.x) access_mode |= X_OK;

  // TODO: Is there anything to do for truncate? We need to be sure we can write the file, but is
  // it even possible to open with O_TRUNC in read-only mode?

  // Normally, faccessat checks whether the real user has access. We want to check as whatever the
  // effective user is. That's the same permission level the build would run with.
  int access_flags = AT_EACCESS;

  // Check access on a symlink if nofollow is specified
  if (_flags.nofollow) access_flags |= AT_SYMLINK_NOFOLLOW;

  // Use faccessat to check the reference
  int rc = faccessat(AT_FDCWD, _path.c_str(), access_mode, access_flags);

  // Check if the access() call failed for some reason
  if (rc) {
    // If the file does not exist, but O_CREAT was included, succeed
    // TODO: Check to be sure we have permission to create the file
    if (errno == ENOENT && _flags.create) return SUCCESS;

    // If we hit this point, it's a normal access and errno has the right code
    return errno;
  } else {
    // If the file exists, but O_CREAT and O_EXCL were passed, fail
    if (_flags.create && _flags.exclusive) return EEXIST;

    // Otherwise, everything is okay
    return SUCCESS;
  }
}

/******************** Eval Methods ********************/

// Check if a reference would resolve the same way on rebuild
bool Predicate::ReferenceResult::eval(map<string, ArtifactVersion>& env) {
  optional<string> path = _ref->getPath();

  // References without paths always succeed
  if (!path.has_value()) return _rc == SUCCESS;

  // If there's a path, check the environment
  auto iter = env.find(path.value());
  if (iter != env.end()) {
    // The reference would succeed
    return _rc == SUCCESS;
  } else {
    return _ref->checkAccess() == _rc;
  }
}

// Check if a MetadataMatch predicate would resolve the same way on rebuild
bool Predicate::MetadataMatch::eval(map<string, ArtifactVersion>& env) {
  optional<string> path = _ref->getPath();

  // References without paths only match if they are in their initial versions
  // TODO: initial version checks are probably a bad hack. Fix this!
  if (!path.has_value()) return _version.getIndex() == 0;

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

  // References without paths only check out if they are in their initial versions
  // TODO: initial version checks are probably a bad hack. Fix this!
  if (!path.has_value()) return _version.getIndex() == 0;

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

/******************** Print Methods ********************/

/// Print a PIPE reference
ostream& Reference::Pipe::print(ostream& o) const {
  return o << getName() << " = PIPE()";
}

/// Print an ACCESS reference
ostream& Reference::Access::print(ostream& o) const {
  return o << getName() << " = ACCESS(\"" << _path << "\", [" << getFlags() << "])";
}

// Print a ReferenceResult predicate
ostream& Predicate::ReferenceResult::print(ostream& o) const {
  // If we can't identify the error code, just print "EMYSTERY"
  string errname = "EMYSTERY";

  // Look up the error name in our map
  auto iter = errors.find(_rc);
  if (iter != errors.end()) {
    errname = iter->second;
  }

  return o << "REFERENCE_RESULT(" << _ref->getName() << ", " << errname << ")";
}

/// Print a METADATA_MATCH predicate
ostream& Predicate::MetadataMatch::print(ostream& o) const {
  return o << "METADATA_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a CONTENTS_MATCH predicate
ostream& Predicate::ContentsMatch::print(ostream& o) const {
  return o << "CONTENTS_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a SET_METADATA action
ostream& Action::SetMetadata::print(ostream& o) const {
  return o << "SET_METADATA(" << _ref->getName() << ", " << _version << ")";
}

/// Print a SET_CONTENTS action
ostream& Action::SetContents::print(ostream& o) const {
  return o << "SET_CONTENTS(" << _ref->getName() << ", " << _version << ")";
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
