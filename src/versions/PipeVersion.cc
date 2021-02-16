#include "PipeVersion.hh"

#include <memory>

using std::shared_ptr;

// Check if a written pipe version matches another version
bool PipeWriteVersion::matches(shared_ptr<ContentVersion> other) noexcept {
  if (!other) return false;
  return other->as<PipeWriteVersion>().get() == this;
}

// Check if a read pipe version matches another version
bool PipeReadVersion::matches(shared_ptr<ContentVersion> other) noexcept {
  if (!other) return false;
  return other->as<PipeReadVersion>().get() == this;
}