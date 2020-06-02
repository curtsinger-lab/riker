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
  virtual void launched(shared_ptr<Command> parent, shared_ptr<Command> child) {
    _commands.insert(child);
    child->emulate(_env);
  }

 private:
  /// The environment used to emulate the build before printing the trace
  Env _env;

  /// The set of all commands
  set<shared_ptr<Command>> _commands;
};
