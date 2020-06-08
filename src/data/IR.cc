#include "IR.hh"

#include <map>
#include <memory>
#include <ostream>
#include <utility>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "artifact/Artifact.hh"
#include "build/Build.hh"
#include "build/BuildObserver.hh"
#include "data/Command.hh"
#include "data/ContentVersion.hh"
#include "data/MetadataVersion.hh"
#include "data/Version.hh"
#include "util/log.hh"

using std::dynamic_pointer_cast;
using std::ostream;
using std::shared_ptr;

void Reference::resolve(shared_ptr<Command> c, Build& build) {
  std::tie(_artifact, _rc) = build.getEnv().get(c, shared_from_this());
}

/******* Emulation *******/

void Reference::emulate(shared_ptr<Command> c, Build& build) {
  // Resolve the reference
  resolve(c, build);
}

void ReferenceResult::emulate(shared_ptr<Command> c, Build& build) {
  // Check if the reference resolved as expected
  if (_ref->getResult() != _rc) build.observeCommandChange(c, shared_from_this());
}

void MetadataMatch::emulate(shared_ptr<Command> c, Build& build) {
  // If the reference does not resolve, report a change
  if (_ref->getResult() != SUCCESS) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Get the latest metadata version. The returned version will be nullptr if no check is necessary.
  auto v = _ref->getArtifact()->accessMetadata(c, _ref);

  // If a version was returned and it doesn't match the expected version, report a mismatch
  if (v && !v->matches(_version)) {
    build.observeMetadataMismatch(c, _ref->getArtifact(), v, _version);
  }
}

void ContentsMatch::emulate(shared_ptr<Command> c, Build& build) {
  // If the reference did not resolve, report a change
  if (_ref->getResult() != SUCCESS) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Get the latest content version. The returned version will be nullptr if no check is necessary.
  auto v = _ref->getArtifact()->accessContents(c, _ref);

  // If a version was returned and it doesn't match the expected version, report a mismatch
  if (v && !v->matches(_version)) {
    build.observeContentMismatch(c, _ref->getArtifact(), v, _version);
  }
}

void SetMetadata::emulate(shared_ptr<Command> c, Build& build) {
  // If the reference did not resolve, report a change
  if (_ref->getResult() != SUCCESS) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  _ref->getArtifact()->setMetadata(c, _ref, _version);
}

void SetContents::emulate(shared_ptr<Command> c, Build& build) {
  // If the reference did not resolve, report a change
  if (_ref->getResult() != SUCCESS) {
    build.observeCommandChange(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  _ref->getArtifact()->setContents(c, _ref, _version);
}

void Launch::emulate(shared_ptr<Command> c, Build& build) {
  // Tell the build to launch the child command
  build.launch(c, _cmd);
}

/******************* Access Methods ********************/

int Access::open() const {
  auto [open_flags, open_mode] = _flags.toOpen();
  return ::open(_path.c_str(), open_flags, open_mode);
}

int Access::stat(struct stat* statbuf) const {
  return fstatat(AT_FDCWD, _path.c_str(), statbuf, _flags.toStat());
}

int Access::access() const {
  auto [access_mode, access_flags] = _flags.toAccess();
  return faccessat(AT_FDCWD, _path.c_str(), access_mode, access_flags);
}

/******************** Print Methods ********************/

// Set up a map from return codes to names
static map<int8_t, string> errors = {{SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
                                     {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
                                     {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}};

/// Print a PIPE reference
ostream& Pipe::print(ostream& o) const {
  return o << getName() << " = PIPE()";
}

/// Print an ACCESS reference
ostream& Access::print(ostream& o) const {
  return o << getName() << " = ACCESS(\"" << _path << "\", [" << getFlags() << "])";
}

// Print a ReferenceResult predicate
ostream& ReferenceResult::print(ostream& o) const {
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
ostream& MetadataMatch::print(ostream& o) const {
  return o << "METADATA_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a CONTENTS_MATCH predicate
ostream& ContentsMatch::print(ostream& o) const {
  return o << "CONTENTS_MATCH(" << _ref->getName() << ", " << _version << ")";
}

/// Print a SET_METADATA action
ostream& SetMetadata::print(ostream& o) const {
  return o << "SET_METADATA(" << getReference()->getName() << ", " << _version << ")";
}

/// Print a SET_CONTENTS action
ostream& SetContents::print(ostream& o) const {
  return o << "SET_CONTENTS(" << getReference()->getName() << ", " << _version << ")";
}

// Print a launch action
ostream& Launch::print(ostream& o) const {
  o << "LAUNCH(" << _cmd << ", [";
  bool first = true;
  for (auto& entry : _cmd->getInitialFDs()) {
    if (!first) o << ", ";
    first = false;
    o << entry.second.getReference()->getName();
  }
  return o << "])";
}
