#pragma once

#include <memory>

using std::shared_ptr;

class Artifact;
class Command;
class Predicate;
class Step;
class Version;

/**
 * This serves as a base class for any utility that needs dependency and change information produced
 * by walking through a build trace. The primary use of this visitor is for the RebuildPlanner
 * class; this class tracks command dependencies and changed predicates to select the set of
 * commands that will run during a rebuild.
 */
class BuildObserver {
 public:
  /// Virtual destructor
  virtual ~BuildObserver() = default;

  /// Command c modifies the metadata for artifact a, creating version v
  virtual void metadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a,
                              shared_ptr<Version> v) {}

  /// Command c modifies the contents of artifact a, creating version v
  virtual void contentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a, shared_ptr<Version> v) {
  }

  /// Command c depends on the metadata for artifact a, accessing version v
  virtual void metadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a, shared_ptr<Version> v) {
  }

  /// Command c depends on the contents of artifact a, accessing version v
  virtual void contentInput(shared_ptr<Command> c, shared_ptr<Artifact> a, shared_ptr<Version> v) {}

  /// Command c does not find the expected metadata in an artifact a
  /// Found version `observed` rather than `expected`
  virtual void metadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a,
                                shared_ptr<Version> observed, shared_ptr<Version> expected) {}

  /// Command c does not find the expected contents in an artifact a
  virtual void contentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a,
                               shared_ptr<Version> observed, shared_ptr<Version> expected) {}

  /// A command has never been run
  virtual void commandNeverRun(shared_ptr<Command> c) {}

  /// The outcome of an IR step has changed since the build trace was collected
  virtual void commandChanged(shared_ptr<Command> c, shared_ptr<const Step> s) {}

  /// The root command is being launched
  virtual void launchRootCommand(shared_ptr<Command> root) {}

  /// A child command is being launched
  virtual void launchChildCommand(shared_ptr<Command> parent, shared_ptr<Command> child) {}

  /// The metadata for an artifact on the filesystem do not match its state at the end of the build.
  /// The build produced `observed`, which does not match the on-disk version `expected`
  virtual void finalMetadataMismatch(shared_ptr<Artifact> a, shared_ptr<Version> observed,
                                     shared_ptr<Version> expected) {}

  /// The contents of an artifact on the filesystem do not match its state at the end of the build.
  /// Th ebuild produced `observed`, which does not match the on-disk version `expected
  virtual void finalContentMismatch(shared_ptr<Artifact> a, shared_ptr<Version> observed,
                                    shared_ptr<Version> expected) {}
};