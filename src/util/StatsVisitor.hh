#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "data/Command.hh"
#include "data/Version.hh"
#include "rebuild/Env.hh"
#include "util/DependencyVisitor.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class StatsVisitor : private DependencyVisitor {
 public:
  /**
   * Gather statistics for a completed build
   * \param b               The build to analyze
   * \param list_artifacts  If true, include a list of artifacts and versions in the final output
   */
  StatsVisitor(shared_ptr<Command> root, bool list_artifacts) : _list_artifacts(list_artifacts) {
    // Get stats from the root command
    processCommand(root);

    // Total up the versions of all artifacts
    for (auto& a : _artifacts) {
      _version_count += a->getVersions().size();
    }
  }

  /// Print the results of our stats gathering
  void print(ostream& o) {
    o << "Build Statistics:" << endl;
    o << "  Commands: " << _command_count << endl;
    o << "  Steps: " << _step_count << endl;
    o << "  Artifacts: " << _artifacts.size() << endl;
    o << "  Artifact Versions: " << _version_count << endl;

    if (_list_artifacts) {
      o << endl;
      o << "Artifacts:" << endl;
      for (auto& a : _artifacts) {
        o << "  " << a->getPath().value_or("<anonymous>") << endl;

        size_t index = 0;
        for (auto& v : a->getVersions()) {
          bool metadata = v->hasMetadata();
          bool fingerprint = v->hasFingerprint();
          bool saved = v->isSaved();

          o << "    v" << index << ":";
          if (metadata) o << " metadata";
          if (fingerprint) o << " fingerprint";
          if (saved) o << " saved";
          if (!metadata && !fingerprint && !saved) o << " no data";
          o << endl;

          index++;
        }
      }
    }
  }

  friend ostream& operator<<(ostream& o, StatsVisitor v) {
    v.print(o);
    return o;
  }

 private:
  /// Gather stats for a command
  void processCommand(shared_ptr<Command> c) {
    // Count this command and its steps
    _command_count++;
    _step_count += c->getSteps().size();

    // Emulate the command to traverse the build trace
    c->emulate(_env, *this);
  }

  /// Called during emulation to report an output from command c
  virtual void addOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {
    _artifacts.insert(a);
  }

  /// Called during emulation to report an input to command c
  virtual void addInput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {
    _artifacts.insert(a);
  }

  /// Called each time a command emulates a launch step
  virtual void launched(shared_ptr<Command> parent, shared_ptr<Command> child) override {
    // Process the child command
    processCommand(child);
  }

 private:
  bool _list_artifacts;  //< Should the stats include a list of artifacts?

  /// The environment used to emulate the build trace
  Env _env;

  /// The total number of commands in the build trace
  size_t _command_count = 0;

  /// The total number of IR steps in the build trace
  size_t _step_count = 0;

  /// The total number of versions of artifacts
  size_t _version_count = 0;

  /// A set of artifacts visited during the stats collection
  set<shared_ptr<Artifact>> _artifacts;
};
