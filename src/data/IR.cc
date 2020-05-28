#include "IR.hh"

#include <cerrno>
#include <map>
#include <memory>
#include <ostream>
#include <utility>

#include "data/Command.hh"
#include "data/Version.hh"
#include "rebuild/Env.hh"
#include "rebuild/Rebuild.hh"

using std::dynamic_pointer_cast;
using std::ostream;

/******* Change Detection *******/

bool Pipe::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  // Nothing to do for pipes
  return true;
}

bool Access::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  // Nothing to do for access
  return true;
}

bool ReferenceResult::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  // Check if the reference resolves the same way
  auto [artifact, rc, created] = env.get(c, _ref);

  // If the reference succeeds, this command depends on the artifact
  if (rc == SUCCESS) {
    r.recordDependency(c, artifact);
  }

  return rc == _rc;
}

bool MetadataMatch::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) return false;

  // If the resolved artifact has no versions, report a change
  if (!a->getLatestVersion()) return false;

  // Record command c's dependency on artifact a
  r.recordDependency(c, a);

  // Compare versions and return the result
  return _version->metadataMatch(a->getLatestVersion());
}

bool ContentsMatch::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) return false;

  // If the resolved artifact has no versions, report a change
  if (!a->getLatestVersion()) return false;

  // Record command c's dependency on artifact a
  r.recordDependency(c, a);

  // Compare versions and return the result
  return _version->fingerprintMatch(a->getLatestVersion());
}

bool SetMetadata::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) return false;

  // Add the assigned version to the artifact
  a->appendVersion(_version, c);

  // This IR step ran as expected
  return true;
}

bool SetContents::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  auto [a, rc, created] = env.get(c, _ref);

  // If the reference does not resolve, report a change
  if (rc != SUCCESS) return false;

  // Add the assigned version to the artifact
  a->appendVersion(_version, c);

  // This IR step ran as expected
  return true;
}

bool Launch::check(shared_ptr<Command> c, Env& env, Rebuild& r) const {
  // Do nothing for launch actions
  return true;
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
