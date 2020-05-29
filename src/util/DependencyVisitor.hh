#pragma once

#include <memory>

using std::shared_ptr;

class Artifact;
class Command;
class Predicate;

/**
 * This serves as a base class for any utility that needs dependency and change information produced
 * by walking through a build trace. The primary use of this visitor is for the Rebuild class; this
 * class tracks command dependencies and changed predicates to select the set of commands that will
 * run during a rebuild.
 */
class DependencyVisitor {
 public:
  /// Command c modifies artifact a
  virtual void addOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c depends on artifact a
  virtual void addInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// The outcome of an IR step has changed since the build trace was collected
  virtual void changed(shared_ptr<Command> c, shared_ptr<const Step> s) {}
};