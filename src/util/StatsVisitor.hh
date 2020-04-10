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

class StatsVisitor {
 public:
  void print(ostream& o, bool list_artifacts) {
    o << "Build Statistics:" << endl;
    o << "  Commands: " << _command_count << endl;
    o << "  Steps: " << _step_count << endl;
    o << "  Artifacts: " << _artifact_count << endl;
    o << "  Artifact Versions: " << _artifact_version_count << endl;

    if (list_artifacts) {
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

  void visit(Build& b) {
    visit(b.getRoot());
    // TODO: visit initial references
  }

  void visit(shared_ptr<Command> c) {
    _command_count++;
    for (auto s : c->getSteps()) {
      visit(c, s);
    }
  };

  void visit(shared_ptr<Command> c, shared_ptr<Step> s) {
    _step_count++;

    if (auto x = dynamic_pointer_cast<Action::Launch>(s)) {
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
    auto [iter, added] = _visited_artifacts.insert(a);
    if (added) {
      _artifact_count++;
      _artifact_version_count += a->getVersionCount();
    }
  }

 private:
  size_t _command_count = 0;
  size_t _step_count = 0;
  size_t _artifact_count = 0;
  size_t _artifact_version_count = 0;

  set<shared_ptr<Artifact>> _visited_artifacts;
};
