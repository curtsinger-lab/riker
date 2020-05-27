#include "Artifact.hh"

/// Get the path to this artifact, if it has one.
/// This is ONLY useful for pretty printing artifacts; the actual path(s) to this artifact can
/// change during a build.
optional<string> Artifact::getPath() const {
  if (auto a = dynamic_pointer_cast<Access>(_ref)) {
    return a->getPath();
  } else {
    return nullopt;
  }
}
