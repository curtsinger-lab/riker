#include "IR.hh"

#include <cerrno>
#include <map>
#include <memory>
#include <ostream>
#include <utility>

#include "data/Command.hh"
#include "data/Version.hh"

using std::dynamic_pointer_cast;
using std::ostream;

static optional<struct stat> get_metadata(shared_ptr<Reference> ref) {
  auto a = dynamic_pointer_cast<Access>(ref);
  if (!a) return nullopt;

  struct stat statbuf;
  int rc;
  if (a->getFlags().nofollow) {
    rc = lstat(a->getPath().c_str(), &statbuf);
  } else {
    rc = stat(a->getPath().c_str(), &statbuf);
  }

  if (rc == 0) {
    return statbuf;
  } else {
    return nullopt;
  }
}

void SetMetadata::saveMetadata() {
  _metadata = get_metadata(_ref);
}

void SetContents::saveMetadata() {
  _metadata = get_metadata(_ref);
}

// Set up a map from return codes to names
static map<int, string> errors = {{SUCCESS, "SUCCESS"}, {EACCES, "EACCES"}, {EDQUOT, "EDQUOT"},
                                  {EEXIST, "EEXIST"},   {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"},
                                  {ELOOP, "ELOOP"},     {ENOENT, "ENOENT"}};

/******************** Print Methods ********************/

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
  return o << "SET_METADATA(" << _ref->getName() << ", v" << getVersionNumber() << ")";
}

/// Print a SET_CONTENTS action
ostream& SetContents::print(ostream& o) const {
  return o << "SET_CONTENTS(" << _ref->getName() << ", v" << getVersionNumber() << ")";
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
