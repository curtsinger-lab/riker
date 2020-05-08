#include "Artifact.hh"

#include <memory>
#include <optional>

#include "data/Version.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::optional;
using std::shared_ptr;

// Get a reference to the latest version of an artifact
shared_ptr<Version> Artifact::getLatestVersion() {
  if (!_latest) tagNewVersion();
  return _latest;
}

// Save a new version of an artifact
shared_ptr<Version> Artifact::tagNewVersion(shared_ptr<Command> creator) {
  optional<string> path;
  if (_path != "") path = _path;

  if (!_latest) {
    _latest = make_shared<Version>(path, creator);
  } else {
    auto v = make_shared<Version>(path, creator);
    _latest->followedBy(v);
    _latest = v;
  }

  return _latest;
}
