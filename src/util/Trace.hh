#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "data/IR.hh"
#include "data/Version.hh"
#include "util/BuildObserver.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class Trace : private BuildObserver {
 public:
  /**
   * Print the complete trace for a build
   * \param b               The build to print
   */
  Trace(shared_ptr<Command> root) : _env(*this) {
    // Save the root command
    _commands.insert(root);

    // Emulate the whole build to reconstruct artifacts
    root->emulate(_env);
  }

  /// Print the trace from the given build
  void print(ostream& o) {
    for (auto& c : _commands) {
      o << c << endl;
      for (auto& s : c->getSteps()) {
        o << "  " << s << endl;
      }
    }
  }

  friend ostream& operator<<(ostream& o, Trace v) {
    v.print(o);
    return o;
  }

 private:
  /// Command c modifies the metadata for artifact a (unused)
  virtual void addMetadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {}

  /// Command c modifies the contents of artifact a (unused)
  virtual void addContentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {}

  /// Command c depends on the metadata for artifact a (unused)
  virtual void addMetadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {}

  /// Command c depends on the contents of artifact a (unused)
  virtual void addContentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) override {}

  /// Command c does not find the expected version of an artifact (unused)
  virtual void mismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) override {}

  /// The outcome of an IR step has changed since the build trace was collected (unused)
  virtual void changed(shared_ptr<Command> c, shared_ptr<const Step> s) override {}

  /// A command is about to be launched. The visitor can choose whether or not to emulate it
  virtual void launched(shared_ptr<Command> parent, shared_ptr<Command> child) override {
    _commands.insert(child);
    child->emulate(_env);
  }

  /// The metadata for an artifact on the file system do not match its state at the end of the build
  /// (unused)
  virtual void finalMetadataMismatch(shared_ptr<Artifact> a) override {}

  /// The contents of an artifact on the file system do not match its state at the end of the build
  /// (unused)
  virtual void finalContentMismatch(shared_ptr<Artifact> a) override {}

 private:
  /// The environment used to emulate the build before printing the trace
  Env _env;

  /// The set of all commands
  set<shared_ptr<Command>> _commands;
};
