#include "Artifact.hh"

#include <memory>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/Version.hh"
#include "ui/log.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::shared_ptr;

// Check if an artifact is a system file
bool Artifact::isSystemFile() const {
  for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
    // Check if the path begins with one of our prefixes.
    // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
    if (_path.rfind(p, 0) != string::npos) return true;
  }
  return false;
}

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
    _latest->_index = 0;
  } else {
    auto v = make_shared<Version>(path, creator);
    v->_previous = _latest;
    v->_index = _latest->_index + 1;
    _latest->_next = v;
    _latest = v;
  }

  return _latest;
}

list<shared_ptr<Version>> Artifact::getVersions() const {
  list<shared_ptr<Version>> result;
  auto current = _latest;
  while (current) {
    result.push_front(current);
    current = current->getPrevious();
  }
  return result;
}
