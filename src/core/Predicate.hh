#pragma once

#include <map>
#include <memory>
#include <ostream>

#include "core/Artifact.hh"
#include "core/Ref.hh"

using std::map;
using std::ostream;
using std::shared_ptr;

// Predicate cases:
//  IS_OK(r : Ref)
//  IS_ERROR(r : Ref, n : int)
//  METADATA_MATCH(r : Ref, v : Artifact::VersionRef)
//  CONTENTS_MATCH(r : Ref, v : Artifact::VersionRef)

class Predicate {
 public:
  class IsOK;
  class IsError;
  class MetadataMatch;
  class ContentsMatch;

  virtual ~Predicate() = default;

  virtual ostream& print(ostream&) const = 0;

  /// Print a Predicate to an output stream
  friend ostream& operator<<(ostream& o, const Predicate& p) { return p.print(o); }

  /// Print a Command* to an output stream
  friend ostream& operator<<(ostream& o, const Predicate* p) { return o << *p; }
};

class Predicate::IsOK : public Predicate {
 public:
  IsOK(shared_ptr<Ref> ref) : _ref(ref) {}

  virtual ~IsOK() = default;

  virtual ostream& print(ostream& o) const { return o << "IS_OK(r" << _ref->getID() << ")"; }

 private:
  shared_ptr<Ref> _ref;
};

class Predicate::IsError : public Predicate {
 public:
  IsError(shared_ptr<Ref> ref, int err) : _ref(ref), _err(err) {}

  virtual ~IsError() = default;

  virtual ostream& print(ostream& o) const {
    static map<int, string> errors = {{EACCES, "EACCES"}, {EDQUOT, "EDQUOT"}, {EEXIST, "EEXIST"},
                                      {EINVAL, "EINVAL"}, {EISDIR, "EISDIR"}, {ELOOP, "ELOOP"},
                                      {ENOENT, "ENOENT"}};

    string errname = "EMYSTERY";

    auto iter = errors.find(-_err);
    if (iter != errors.end()) {
      errname = iter->second;
    }

    return o << "IS_ERROR(r" << _ref->getID() << ", " << errname << ")";
  }

 private:
  shared_ptr<Ref> _ref;
  int _err;
};

class Predicate::MetadataMatch : public Predicate {
 public:
  MetadataMatch(shared_ptr<Ref> ref, Artifact::VersionRef version) : _ref(ref), _version(version) {}

  virtual ~MetadataMatch() = default;

  virtual ostream& print(ostream& o) const {
    return o << "METADATA_MATCH(r" << _ref->getID() << ", " << _version << ")";
  }

 private:
  shared_ptr<Ref> _ref;
  Artifact::VersionRef _version;
};

class Predicate::ContentsMatch : public Predicate {
 public:
  ContentsMatch(shared_ptr<Ref> ref, Artifact::VersionRef version) : _ref(ref), _version(version) {}

  virtual ~ContentsMatch() = default;

  virtual ostream& print(ostream& o) const {
    return o << "CONTENTS_MATCH(r" << _ref->getID() << ", " << _version << ")";
  }

 private:
  shared_ptr<Ref> _ref;
  Artifact::VersionRef _version;
};
