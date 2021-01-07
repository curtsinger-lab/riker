#include "PipeVersion.hh"

#include <memory>

#include "runtime/Build.hh"
#include "versions/FileVersion.hh"

using std::shared_ptr;

class Command;
class Env;

// Check if a written pipe version matches another version
bool PipeWriteVersion::matches(shared_ptr<ContentVersion> other) const noexcept {
  return other->as<PipeWriteVersion>().get() == this;
}

// Check if a read pipe version matches another version
bool PipeReadVersion::matches(shared_ptr<ContentVersion> other) const noexcept {
  // Cast the other version to a PipeReadVersion
  auto r = other->as<PipeReadVersion>();
  if (!r) return false;

  // Are the two versions the same instance?
  if (r.get() == this) return true;

  // Do the two sets of reads observe the same number of writes?
  if (_observed.size() != r->_observed.size()) return false;

  // Walk through and compare all the observed writes
  auto self_iter = _observed.begin();
  auto other_iter = r->_observed.begin();

  while (self_iter != _observed.end()) {
    auto self_write = *self_iter;
    auto other_write = *other_iter;
    if (!self_write->matches(other_write)) return false;
    self_iter++;
    other_iter++;
  }

  return true;
}