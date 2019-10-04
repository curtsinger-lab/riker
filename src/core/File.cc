#include "core/File.hh"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <memory>
#include <string>
#include <vector>

#include <dirent.h>
#include <fcntl.h>
#include <linux/magic.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <unistd.h>

#include <capnp/blob.h>
#include <kj/string.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "db/db.capnp.h"
#include "fingerprint/blake2.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::make_shared;
using std::ostream;
using std::shared_ptr;

size_t File::next_id = 0;

ostream& operator<<(ostream& o, const File* f) {
  string type = "File";
  
  if (f->getType() == File::Type::PIPE) {
    type = "Pipe";
  } else if (f->getType() == File::Type::DIRECTORY) {
    type = "Dir";
  }
  
  o << "[" << type;
  if (f->getPath() != "") o << " " << f->getPath();
  o << "]";
  return o;
}

ostream& operator<<(ostream& o, const File::Version& v) {
  return o << v.getFile() << "@" << v.getIndex();
}

void File::createdBy(Command* c) {
  // Tag a new created version
  auto v = makeVersion(Version::Action::CREATE, c);

  // Record the output edge from the command
  if (c->addOutput(v)) INFO << v << " created by " << c;
}

void File::readBy(Command* c) {
  // If this file has no previous versions, tag a version that references an existing file
  if (_versions.size() == 0) {
    // A reference version has no creator
    auto v = makeVersion(Version::Action::REFERENCE);
  }

  // Record the dependency
  if (c->addInput(_versions.back())) INFO << c << " read " << _versions.back();
}

void File::writtenBy(Command* c) {
  // There must be a previous version if we're writing a file. If the first action performed on a
  // file is to write to it, there will be a create, reference, or truncate version already.
  FAIL_IF(_versions.size() == 0) << "Invalid write to file with no prior version: " << _path;

  // If the previous version was a write by this command, we don't need to tag a new version
  if (_versions.back()._action == Version::Action::WRITE && _versions.back()._writer == c) {
    return;
  }

  // Otherwise we tag a new written version
  auto v = makeVersion(Version::Action::WRITE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " wrote " << v;
}

void File::truncatedBy(Command* c) {
  // Tag a truncated version
  auto v = makeVersion(Version::Action::TRUNCATE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " truncated " << v;
}

void File::deletedBy(Command* c) {
  // Tag a deleted version
  auto v = makeVersion(Version::Action::DELETE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " deleted " << v;
}

void File::serialize(Serializer& serializer, db::File::Builder builder) {}
