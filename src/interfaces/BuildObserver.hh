#pragma once

#include <memory>

#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/CommandRun.hh"

using std::shared_ptr;
using std::unique_ptr;

class Artifact;
class Command;
class Predicate;
class Ref;
class Step;
class Version;

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

  /// A command is being launched. The parent will be null if this is the root command.
  virtual void observeLaunch(const shared_ptr<Command>& parent,
                             const shared_ptr<Command>& child) noexcept {}
};