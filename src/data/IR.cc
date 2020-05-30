#include "IR.hh"

#include <map>
#include <memory>
#include <ostream>
#include <utility>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "data/Command.hh"
#include "data/Version.hh"
#include "rebuild/Artifact.hh"
#include "rebuild/Env.hh"
#include "ui/log.hh"
#include "util/DependencyVisitor.hh"

using std::dynamic_pointer_cast;
using std::ostream;
using std::shared_ptr;

/******* Change Detection *******/

void Pipe::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  // Resolve the reference
  auto [artifact, rc, created] = env.get(c, shared_from_this());

  // Nothing else to do for pipes; referencing them always creates them.
  // This IR step should be followed by a SET_CONTENTS step soon after.
}

void Access::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  // Resolve the reference
  auto [artifact, rc, created] = env.get(c, shared_from_this());

  // Command c depends on the artifact if it couldn't create it with the reference.
  // This is true regardless of whether or not the access actually created the artifact.
  if (rc == SUCCESS && !_flags.create) {
    v.addInput(c, artifact);
  }
}

void ReferenceResult::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  // Check if the reference resolves the same way
  auto [artifact, rc, created] = env.get(c, _ref);

  if (rc != _rc) v.changed(c, shared_from_this());
}

void MetadataMatch::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) {
    v.changed(c, shared_from_this());
    return;
  }

  // If the resolved artifact has no versions, report a change
  if (!a->getLatestVersion()) {
    v.changed(c, shared_from_this());
    return;
  }

  // Record command c's dependency on artifact a
  v.addInput(c, a);

  // Compare versions and report a change if detected
  if (!_version->metadataMatch(a->getLatestVersion())) {
    v.mismatch(a);
    v.changed(c, shared_from_this());
  }
}

void ContentsMatch::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) {
    v.changed(c, shared_from_this());
    return;
  }

  // If the resolved artifact has no versions, report a change
  if (!a->getLatestVersion()) {
    v.changed(c, shared_from_this());
    return;
  }

  // Record command c's dependency on artifact a
  v.addInput(c, a);

  // Compare versions and report a change if detected
  if (!_version->contentsMatch(a->getLatestVersion())) {
    v.mismatch(a);
    v.changed(c, shared_from_this());
  }
}

void SetMetadata::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) {
    v.changed(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  a->appendVersion(_version, c);

  // Report command c's output to artifact a
  v.addOutput(c, a);
}

void SetContents::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) {
    v.changed(c, shared_from_this());
    return;
  }

  // Add the assigned version to the artifact
  a->appendVersion(_version, c);

  // Report command c's output to artifact a
  v.addOutput(c, a);
}

void Launch::emulate(shared_ptr<Command> c, Env& env, DependencyVisitor& v) {
  // Inform the dependency visitor that command c launches a child command
  v.launched(c, _cmd);
}

/******************* Access Methods ********************/

int Access::open() const {
  return ::open(_path.c_str(), _flags.toOpen());
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
static map<int, string> errors = {{SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
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
