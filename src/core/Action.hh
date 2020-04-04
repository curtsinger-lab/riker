#pragma once

#include <memory>
#include <ostream>

#include "core/Artifact.hh"
#include "core/Ref.hh"

using std::ostream;
using std::shared_ptr;

class Command;

// Action cases:
//  LAUNCH(c : Command, file descriptors)
//  SET_METADATA(r : Ref, v : Artifact::VersionRef)
//  SET_CONTENTS(r : Ref, v : Artifact::VersionRef)

class Action {
 public:
  class Launch;
  class SetMetadata;
  class SetContents;

  virtual ~Action() = default;

  virtual ostream& print(ostream&) const = 0;

  /// Print an Action to an output stream
  friend ostream& operator<<(ostream& o, const Action& a) { return a.print(o); }

  /// Print an Action* to an output stream
  friend ostream& operator<<(ostream& o, const Action* a) { return o << *a; }
};

class Action::Launch : public Action {
 public:
  Launch(shared_ptr<Command> cmd) : _cmd(cmd) {}

  virtual ~Launch() = default;

  virtual ostream& print(ostream& o) const;

 private:
  shared_ptr<Command> _cmd;
};

class Action::SetMetadata : public Action {};

class Action::SetContents : public Action {
 public:
  SetContents(shared_ptr<Ref> ref, Artifact::VersionRef version) : _ref(ref), _version(version) {}

  virtual ~SetContents() = default;

  virtual ostream& print(ostream& o) const {
    return o << "SET_CONTENTS(r" << _ref->getID() << ", " << _version << ")";
  }

 private:
  shared_ptr<Ref> _ref;
  Artifact::VersionRef _version;
};
