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
  /// Virtual destructor
  virtual ~BuildObserver() = default;

  /// Command c modifies the metadata for artifact a
  virtual void metadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c modifies the contents of artifact a
  virtual void contentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c depends on the metadata for artifact a
  virtual void metadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c depends on the contents of artifact a
  virtual void contentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c does not find the expected metadata in an artifact a
  virtual void metadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// Command c does not find the expected contents in an artifact a
  virtual void contentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {}

  /// A command has never been run
  virtual void commandNeverRun(shared_ptr<Command> c) {}

  /// The outcome of an IR step has changed since the build trace was collected
  virtual void commandChanged(shared_ptr<Command> c, shared_ptr<const Step> s) {}

  /// The root command is being launched
  virtual void launchRootCommand(shared_ptr<Command> root) {}

  /// A child command is being launched
  virtual void launchChildCommand(shared_ptr<Command> parent, shared_ptr<Command> child) {}

  /// The metadata for an artifact on the file system do not match its state at the end of the build
  virtual void finalMetadataMismatch(shared_ptr<Artifact> a) {}

  /// The contents of an artifact on the file system do not match its state at the end of the build
  virtual void finalContentMismatch(shared_ptr<Artifact> a) {}
};