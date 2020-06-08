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

  /// Command c modifies artifact a, creating version v
  virtual void output(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                      const shared_ptr<Version>& v) {}

  /// Command c depends on artifact a, accessing version v
  virtual void input(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                     const shared_ptr<Version>& v) {}

  /// Command c did not find the expect version of artifact a
  virtual void mismatch(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                        const shared_ptr<Version>& observed, const shared_ptr<Version>& expected) {}

  /// A command has never been run
  virtual void commandNeverRun(const shared_ptr<Command>& c) {}

  /// The outcome of an IR step has changed since the build trace was collected
  virtual void commandChanged(const shared_ptr<Command>& c, const shared_ptr<const Step>& s) {}

  /// The root command is being launched
  virtual void launchRootCommand(const shared_ptr<Command>& root) {}

  /// A child command is being launched
  virtual void launchChildCommand(const shared_ptr<Command>& parent,
                                  const shared_ptr<Command>& child) {}

  /// The stat of an artifact on the filesystem does not match its state at the end of the build.
  /// The build produced `observed`, which does not match the on-disk version `expected`
  virtual void finalMismatch(const shared_ptr<Artifact>& a, const shared_ptr<Version>& observed,
                             const shared_ptr<Version>& expected) {}
};