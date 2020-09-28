#pragma once

#include <memory>

using std::shared_ptr;
using std::unique_ptr;

class Artifact;
class Command;
class Predicate;
class Step;
class Version;

enum class InputType {
  PathResolution,  // The input is a dependency for path resolution
  Accessed,        // The input is accessed directly
  Exists,          // The input must exist, but its specific contents do not matter
};

/**
 * This serves as a base class for any utility that needs dependency and change information
 * produced by walking through a build trace. The primary use of this visitor is for the
 * RebuildPlanner class; this class tracks command dependencies and changed predicates to select
 * the set of commands that will run during a rebuild.
 */
class BuildObserver {
 public:
  /// Virtual destructor
  virtual ~BuildObserver() noexcept = default;

  /// Command c modifies artifact a, creating version v
  virtual void output(shared_ptr<Command> c,
                      shared_ptr<Artifact> a,
                      shared_ptr<Version> v) noexcept {}

  /// Command c depends on artifact a, accessing version v
  virtual void input(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v,
                     InputType t) noexcept {}

  /// Command c did not find the expect version of artifact a
  virtual void mismatch(shared_ptr<Command> c,
                        shared_ptr<Artifact> a,
                        shared_ptr<Version> observed,
                        shared_ptr<Version> expected) noexcept {}

  /// A command has never been run
  virtual void commandNeverRun(shared_ptr<Command> c) noexcept {}

  /// The outcome of an IR step has changed since the build trace was collected
  virtual void commandChanged(shared_ptr<Command> c) noexcept {}

  /// A command is being launched. The parent will be null if this is the root command.
  virtual void launch(shared_ptr<Command> parent, shared_ptr<Command> child) noexcept {}

  /// The stat of an artifact on the filesystem does not match its state at the end of the build.
  /// The build produced version `produced`, which does not match the `ondisk` version.
  virtual void finalMismatch(shared_ptr<Artifact> a,
                             shared_ptr<Version> produced,
                             shared_ptr<Version> ondisk) noexcept {}
};