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

/******************** Eval Methods ********************/

// Create a PIPE reference
bool Reference::Pipe::eval(shared_ptr<CommandEnv> env) {
  return true;
}

// Create an ACCESS reference
bool Reference::Access::eval(shared_ptr<CommandEnv> env) {
  return true;
}

// Check if a reference would resolve the same way on rebuild
bool Predicate::ReferenceResult::eval(shared_ptr<CommandEnv> env) {
  // Check the environment
  return env->checkAccess(_ref, _rc);
}

// Check if a MetadataMatch predicate would resolve the same way on rebuild
bool Predicate::MetadataMatch::eval(shared_ptr<CommandEnv> env) {
  return env->checkMetadataMatch(_ref, _version);
}

// Check if a ContentsMatch predicate would resolve the same ay on rebuild
bool Predicate::ContentsMatch::eval(shared_ptr<CommandEnv> env) {
  return env->checkContentsMatch(_ref, _version);
}

// Run a LAUNCH action
bool Action::Launch::eval(shared_ptr<CommandEnv> env) {
  return true;
}

// Run a SetMetadata action
bool Action::SetMetadata::eval(shared_ptr<CommandEnv> env) {
  env->setMetadata(_ref, _version);
  return true;
}

// Run a SetContents action
bool Action::SetContents::eval(shared_ptr<CommandEnv> env) {
  env->setContents(_ref, _version);
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
