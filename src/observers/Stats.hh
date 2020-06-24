#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "build/BuildObserver.hh"
#include "core/Command.hh"
#include "versions/Version.hh"

using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class Stats final : public BuildObserver {
 public:
  /**
   * Gather statistics from a build as it runs
   * \param list_artifacts  If true, include a list of artifacts and versions in the final output
   */
  Stats(bool list_artifacts) noexcept : _list_artifacts(list_artifacts) {}

  /// Print the results of our stats gathering
  ostream& print(ostream& o) noexcept {
    // Total versions for all artifacts
    size_t version_count = 0;
    for (const auto& a : _artifacts) {
      version_count += a->getVersionCount();
    }

    o << "Build Statistics:" << endl;
    o << "  Commands: " << _command_count << endl;
    o << "  Steps: " << _step_count << endl;
    o << "  Artifacts: " << _artifacts.size() << endl;
    o << "  Artifact Versions: " << version_count << endl;

    if (_list_artifacts) {
      o << endl;
      o << "Artifacts:" << endl;
      for (const auto& a : _artifacts) {
        if (a->getName().empty()) {
          o << "  " << a->getTypeName() << ": <anonymous>" << endl;
        } else {
          o << "  " << a->getTypeName() << ": " << a->getName() << endl;
        }

        size_t index = 0;
        for (const auto& v : a->getVersions()) {
          o << "    v" << index << ": " << v << endl;
          index++;
        }
        o << endl;
      }
    }

    return o;
  }

  /// Print a Stats reference
  friend ostream& operator<<(ostream& o, Stats& s) noexcept { return s.print(o); }

  /// Print a Stats pointer
  friend ostream& operator<<(ostream& o, Stats* s) noexcept { return s->print(o); }

 private:
  /// Gather stats for a command
  void processCommand(shared_ptr<Command> c) noexcept {
    // Count this command and its steps
    _command_count++;
    //_step_count += c->getSteps().size();
  }

  /// Called during emulation to report an output from command c
  virtual void output(shared_ptr<Command> c,
                      shared_ptr<Artifact> a,
                      shared_ptr<Version> v) noexcept override final {
    _artifacts.insert(a);
  }

  /// Called during emulation to report an input to command c
  virtual void input(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v,
                     InputType t) noexcept override final {
    _artifacts.insert(a);
  }

  /// Called each time a command emulates a launch step
  virtual void launch(shared_ptr<Command> parent,
                      shared_ptr<Command> child) noexcept override final {
    // Process the child command
    processCommand(child);
  }

 private:
  /// Should the stats include a list of artifacts?
  bool _list_artifacts;

  /// The total number of commands in the build trace
  size_t _command_count = 0;

  /// The total number of IR steps in the build trace
  size_t _step_count = 0;

  /// A set of artifacts visited during the stats collection
  set<shared_ptr<Artifact>> _artifacts;
};
