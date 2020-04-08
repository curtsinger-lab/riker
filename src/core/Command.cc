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

using std::dynamic_pointer_cast;
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

  // We are rerunning this command, so clear the list of steps
  _steps.clear();

  // Actually run the command
  tracer.run(shared_from_this());
}

/// Print the abstract trace of this command (and its children) to an output stream
void Command::printTrace(ostream& o) const {
  // Print this command's name
  o << this << endl;

  list<shared_ptr<Command>> children;

  // Print the trace for this command
  for (auto& s : _steps) {
    o << "  " << s << endl;

    // If this is a LAUNCH step, we will need to print the child command
    auto launch = std::dynamic_pointer_cast<Action::Launch>(s);
    if (launch) {
      children.push_back(launch->getCommand());
    }
  }

  // Print traces for all child commands
  for (auto& child : children) {
    child->printTrace(o);
  }
}

void Command::drawGraph(Graphviz& g) {
  g.addCommand(shared_from_this());
  for (auto& s : _steps) {
    shared_ptr<Predicate::MetadataMatch> metadata_match;
    shared_ptr<Predicate::ContentsMatch> contents_match;
    shared_ptr<Action::Launch> launch;
    shared_ptr<Action::SetMetadata> set_metadata;
    shared_ptr<Action::SetContents> set_contents;

    if ((metadata_match = dynamic_pointer_cast<Predicate::MetadataMatch>(s))) {
      auto version = metadata_match->getVersion();
      // Should we display the artifact referenced here?
      if (!version.getArtifact()->isSystemFile() || options.show_sysfiles) {
        g.addInputEdge(version, shared_from_this());
      }

    } else if ((contents_match = dynamic_pointer_cast<Predicate::ContentsMatch>(s))) {
      auto version = contents_match->getVersion();
      // Should we display the artifact referenced here?
      if (!version.getArtifact()->isSystemFile() || options.show_sysfiles) {
        g.addInputEdge(version, shared_from_this());
      }

    } else if ((launch = dynamic_pointer_cast<Action::Launch>(s))) {
      // Recursively draw the child command
      launch->getCommand()->drawGraph(g);
      // Add a graph edge to the child command
      g.addCommandEdge(shared_from_this(), launch->getCommand());

    } else if ((set_metadata = dynamic_pointer_cast<Action::SetMetadata>(s))) {
      auto version = set_metadata->getVersion();
      // Should we display the artifact referenced here?
      if (!version.getArtifact()->isSystemFile() || options.show_sysfiles) {
        g.addOutputEdge(shared_from_this(), version);
      }

    } else if ((set_contents = dynamic_pointer_cast<Action::SetContents>(s))) {
      auto version = set_contents->getVersion();
      // Should we display the artifact referenced here?
      if (!version.getArtifact()->isSystemFile() || options.show_sysfiles) {
        g.addOutputEdge(shared_from_this(), version);
      }
    }
  }
}
