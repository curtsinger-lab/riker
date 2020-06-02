#pragma once

#include <memory>

using std::shared_ptr;

class Artifact;
class Command;
class Predicate;
class Step;

/**
 * This serves as a base class for any utility that needs dependency and change information produced
 * by walking through a build trace. The primary use of this visitor is for the Rebuild class; this
 * class tracks command dependencies and changed predicates to select the set of commands that will
 * run during a rebuild.
 */
class BuildObserver {
 public:
  /// Command c modifies the metadata for artifact a
  virtual void addMetadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c modifies the contents of artifact a
  virtual void addContentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c depends on the metadata for artifact a
  virtual void addMetadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c depends on the contents of artifact a
  virtual void addContentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c does not find the expected version of an artifact
  virtual void mismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// The outcome of an IR step has changed since the build trace was collected
  virtual void changed(shared_ptr<Command> c, shared_ptr<const Step> s) {}

  /// A command is about to be launched. The visitor can choose whether or not to emulate it.
  virtual void launched(shared_ptr<Command> parent, shared_ptr<Command> child) {}
};