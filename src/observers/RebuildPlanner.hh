#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <set>

#include "artifact/Artifact.hh"
#include "build/BuildObserver.hh"
#include "data/Command.hh"
#include "ui/options.hh"
#include "util/log.hh"

using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;

/// This class captures all of the logic and state required to plan a rebuild.
class RebuildPlanner : public BuildObserver {
 public:
  /// Create a rebuild planner
  RebuildPlanner() = default;

  // Disallow Copy
  RebuildPlanner(const RebuildPlanner&) = delete;
  RebuildPlanner& operator=(const RebuildPlanner&) = delete;

  // Allow Move
  RebuildPlanner(RebuildPlanner&&) = default;
  RebuildPlanner& operator=(RebuildPlanner&&) = default;

  /**
   * Identify the commands that must rerun and mark them in the given Build
   * \param b The build that will be used to execute any commands this rebuild marks for rerun
   */
  void planBuild(Build& b) const {
    // Mark all the commands with changed inputs
    for (auto& c : _changed) {
      mark(b, c);
    }

    // Mark all the commands whose output is required
    for (auto& c : _output_needed) {
      mark(b, c);
    }
  }

  /// Print information about the rebuild state
  ostream& print(ostream& o) const {
    if (_changed.size() > 0) {
      o << "Commands with changed inputs:" << endl;
      for (auto& c : _changed) {
        o << "  " << c << endl;
      }
      o << endl;
    }

    if (_output_needed.size() > 0) {
      o << "Commands whose output is missing or modified:" << endl;
      for (auto& c : _output_needed) {
        o << "  " << c << endl;
      }
      o << endl;
    }

    return o;
  }

  /// Print a RebuildPlanner reference
  friend ostream& operator<<(ostream& o, const RebuildPlanner& r) { return r.print(o); }

  /// Print a RebuildPlanner pointer
  friend ostream& operator<<(ostream& o, const RebuildPlanner* r) { return r->print(o); }

  /******** BuildObserver Interface ********/

  /// Command c depends on version v of artifact a
  virtual void input(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                     const shared_ptr<Version>& v) override {
    // During the planning phase, record this dependency
    auto creator = a->getContentCreator();
    if (creator) {
      // Output from creator is used by c. If creator reruns, c may have to rerun.
      _output_used_by[creator].insert(c);

      // The dependency back edge depends on caching
      if (options::enable_cache && a->isSaved()) {
        // If this artifact is cached, we could restore it before c runs.
      } else {
        // Otherwise, if c has to run then we also need to run creator to produce this input
        _needs_output_from[c].insert(creator);
      }
    }
  }

  /// Command c did not find the expected version in artifact a
  virtual void mismatch(const shared_ptr<Command>& c, const shared_ptr<Artifact>& a,
                        const shared_ptr<Version>& observed,
                        const shared_ptr<Version>& expected) override {
    // Record the change
    LOG << c << " observed change in " << a;
    _changed.insert(c);
  }

  /// Command c has never been run
  virtual void commandNeverRun(const shared_ptr<Command>& c) override {
    // Record the change
    LOG << c << " never run";
    _changed.insert(c);
  }

  /// IR step s in command c observed a change
  virtual void commandChanged(const shared_ptr<Command>& c,
                              const shared_ptr<const Step>& s) override {
    // Record the change
    LOG << c << " changed: " << s;
    _changed.insert(c);
  }

  /// An artifact's final version does not match what is on the filesystem
  virtual void finalMismatch(const shared_ptr<Artifact>& a, const shared_ptr<Version>& observed,
                             const shared_ptr<Version>& expected) override {
    // If this artifact was not created by any command, there's nothing we can do about it
    if (!a->getContentCreator()) return;

    // If this artifact's final version is cached, we can just stage it in
    if (options::enable_cache && a->isSaved()) return;

    // Otherwise we have to run the command that created this artifact
    _output_needed.insert(a->getContentCreator());
  }

 private:
  /// Mark a command for rerun, and propagate that marking to its dependencies/dependents
  void mark(Build& b, const shared_ptr<Command>& c) const {
    // Mark command c for rerun. If the command was already marked, setRerun will return false and
    // we can stop here
    if (!b.setRerun(c)) return;

    // Mark this command's children
    for (auto& child : c->getChildren()) {
      mark(b, child);
    }

    // Mark any commands that produce output that this command needs
    if (auto iter = _needs_output_from.find(c); iter != _needs_output_from.end()) {
      for (auto& other : iter->second) {
        mark(b, other);
      }
    }

    // Mark any commands that use this command's output
    if (auto iter = _output_used_by.find(c); iter != _output_used_by.end()) {
      for (auto& other : iter->second) {
        mark(b, other);
      }
    }
  }

 private:
  /// Track commands with changed inputs
  set<shared_ptr<Command>> _changed;

  /// Track commands whose output is needed
  set<shared_ptr<Command>> _output_needed;

  /// Map command that produces output(s) -> commands that consume that output
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _output_used_by;

  /// Map command that consumes uncached input -> commands that produce that input
  map<shared_ptr<Command>, set<shared_ptr<Command>>> _needs_output_from;
};
