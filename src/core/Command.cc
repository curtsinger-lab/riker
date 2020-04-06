#include "Command.hh"

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "core/Artifact.hh"
#include "core/IR.hh"
#include "tracing/Tracer.hh"
#include "ui/Graphviz.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::map;
using std::shared_ptr;
using std::string;
using std::vector;

string Command::getShortName() const {
  auto base = _exe;
  if (_args.size() > 0) base = _args.front();

  auto pos = base.rfind('/');
  if (pos == string::npos) {
    return base;
  } else {
    return base.substr(pos + 1);
  }
}

string Command::getFullName() const {
  string result;
  for (const string& arg : _args) {
    result += arg + " ";
  }
  return result;
}

void Command::run(Tracer& tracer) {
  // TODO: checking logic goes here
  // simulate all of the steps:
  //   references: check that access remains the same
  //   predicates: still hold
  //   action: simulate effect of actions
  //     two things to check:
  //     1. whether the action had the same effect as before
  //     2. if the action had the same effect, what the effect actually is
  //     NOTE: use recursive state environment
  tracer.run(shared_from_this());
}

void Command::drawGraph(Graphviz& g) {
  /*g.addCommand(shared_from_this());
  for (auto f : _inputs) {
    if (!f.getArtifact()->isSystemFile() || options.show_sysfiles) {
      g.addInputEdge(f, shared_from_this());
    }
  }
  for (auto f : _outputs) {
    if (!f.getArtifact()->isSystemFile() || options.show_sysfiles) {
      g.addOutputEdge(shared_from_this(), f);
    }
  }
  for (auto& c : _children) {
    c->drawGraph(g);
    g.addCommandEdge(shared_from_this(), c);
  }*/
}
