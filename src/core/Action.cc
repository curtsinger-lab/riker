#include "Action.hh"

#include "core/Command.hh"

ostream& Action::Launch::print(ostream& o) const {
  o << "LAUNCH(" << _cmd << ", [";
  bool first = true;
  for (auto& entry : _cmd->getInitialFDs()) {
    if (!first) o << ", ";
    first = false;
    o << "r" << entry.second->getID();
  }
  return o << "])";
}
