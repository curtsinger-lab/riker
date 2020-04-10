#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "core/Build.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class StatsVisitor {
 public:
  /**
   * Gather statistics for a completed build
   * \param b               The build to analyze
   * \param list_artifacts  If true, include a list of artifacts and versions in the final output
   */
  StatsVisitor(Build& b, bool list_artifacts) : _list_artifacts(list_artifacts) {
    visit(b.getRoot());
    // TODO: visit initial references
  }

  /// Print the results of our stats gathering
  void print(ostream& o) {
    o << "Build Statistics:" << endl;
    o << "  Commands: " << _command_count << endl;
    o << "  Steps: " << _step_count << endl;
    o << "  Artifacts: " << _artifact_count << endl;
    o << "  Artifact Versions: " << _artifact_version_count << endl;

    if (_list_artifacts) {
      o << endl;
      o << "Artifacts:" << endl;
      for (auto a : _visited_artifacts) {
        size_t skipped = 0;
        o << "  " << a << endl;
        for (auto v : a->getVersions()) {
          bool metadata = v.hasMetadata();
          bool fingerprint = v.hasFingerprint();
          bool contents = v.hasSavedContents();

          if (metadata || fingerprint || contents) {
            o << "    v" << v.getIndex() << ":";
            if (metadata) o << " metadata";
            if (fingerprint) o << " fingerprint";
            if (contents) o << " contents";
            o << endl;
          } else {
            skipped++;
          }
        }
        if (skipped > 0) {
          o << "    (skipped " << skipped << " version" << (skipped > 1 ? "s" : "")
            << " with no saved data)" << endl;
        }
      }
    }
  }

  friend ostream& operator<<(ostream& o, StatsVisitor v) {
    v.print(o);
    return o;
  }

 private:
  void visit(shared_ptr<Command> c) {
    // Count this command
    _command_count++;

    // Visit each of the steps the command runs
    for (auto s : c->getSteps()) {
      visit(s);
    }
  };

  void visit(shared_ptr<Step> s) {
    // Count this step
    _step_count++;

    // Handle steps that launch new commands or access artifacts
    if (auto x = dynamic_pointer_cast<Action::Launch>(s)) {
      // Recurse into the launched command
      visit(x->getCommand());

    } else if (auto x = dynamic_pointer_cast<Predicate::MetadataMatch>(s)) {
      visit(x->getVersion());

    } else if (auto x = dynamic_pointer_cast<Predicate::ContentsMatch>(s)) {
      visit(x->getVersion());

    } else if (auto x = dynamic_pointer_cast<Action::SetMetadata>(s)) {
      visit(x->getVersion());

    } else if (auto x = dynamic_pointer_cast<Action::SetContents>(s)) {
      visit(x->getVersion());
    }
  }

  void visit(ArtifactVersion v) { visit(v.getArtifact()); }

  void visit(shared_ptr<Artifact> a) {
    // set.insert() returns pair(iter, true) if the item is new.
    // Use structured binding decl to access
    auto [_, added] = _visited_artifacts.insert(a);
    if (added) {
      // Count this artifact if it's new
      _artifact_count++;
      _artifact_version_count += a->getVersionCount();
    }
  }

 private:
  bool _list_artifacts;  //< Should the stats include a list of artifacts?

  size_t _command_count = 0;           //< The total number of commands in the build
  size_t _step_count = 0;              //< The total number of steps in the build
  size_t _artifact_count = 0;          //< The total number of artifacts in the build
  size_t _artifact_version_count = 0;  //< The total number of artifact versions in the build

  /// A set of artifacts that have already been visited
  set<shared_ptr<Artifact>> _visited_artifacts;
};
