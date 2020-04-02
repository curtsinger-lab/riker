#include "core/Command.hh"

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "core/Artifact.hh"
#include "core/Ref.hh"
#include "tracing/Tracer.hh"
#include "ui/Graphviz.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::map;
using std::shared_ptr;
using std::string;
using std::vector;

size_t Command::next_id = 0;

// TODO: This is gross. Move it somewhere better or get rid of it.
size_t Ref::next_id = 0;

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

shared_ptr<Command> Command::createChild(string exe, vector<string> args) {
  _children.emplace_back(new Command(exe, args, shared_from_this()));
  auto child = _children.back();

  INFO << this << " starting child " << child;
  if (args.size() > 0) {
    LOG << "  " << exe << " (" << args.front() << ")";
  } else {
    LOG << "  " << exe;
  }

  bool first = true;
  for (auto& arg : args) {
    if (!first) LOG << "    " << arg;
    first = false;
  }

  return child;
}

bool Command::addInput(Artifact::VersionRef f) {
  if (_inputs.find(f) != _inputs.end()) return false;
  _inputs.insert(f);
  return true;
}

bool Command::addOutput(Artifact::VersionRef f) {
  if (_outputs.find(f) != _outputs.end()) return false;
  _outputs.insert(f);
  return true;
}

void Command::run(Tracer& tracer) {
  tracer.run(shared_from_this());
}

bool Command::prune() {
  // Recursively prune in child commands, potentially removing the whole command
  for (auto iter = _children.begin(); iter != _children.end();) {
    auto& child = *iter;
    if (child->prune()) {
      iter = _children.erase(iter);
    } else {
      ++iter;
    }
  }

  // If this command has no children and no outputs, we can prune it
  return _outputs.size() == 0 && _children.size() == 0;
}

void Command::drawGraph(Graphviz& g) {
  g.addCommand(shared_from_this());
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
  }
}
