#include "Artifact.hh"

#include <sys/stat.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

void Artifact::fingerprint() {
  // If there are no references to this file, there's no need to fingerprint
  if (_versions.size() == 0) return;

  // Get the record for the latest version
  Version& r = _versions.back();

  // If we already have a fingerprint, bail out
  if (r.has_fingerprint) return;

  // Stat the file
  if (stat(_path.c_str(), &r.metadata) == 0) {
    r.has_metadata = true;
  } else {
    WARN << "Unable to stat artifact " << _path;
  }
}
