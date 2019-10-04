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
using std::shared_ptr;

void File::createdBy(shared_ptr<Command> c) {
  // Tag a new created version
  auto v = Version::make_shared(Version::Action::CREATE, c);
  _versions.push_back(v);

  // Record the output edge from the command
  c->addOutput(v);
}

void File::readBy(shared_ptr<Command> c) {
  // If this file has no previous versions, tag a version that references an existing file
  if (_versions.size() == 0) {
    // A reference version has no creator
    auto v = Version::make_shared(Version::Action::REFERENCE);
    _versions.push_back(v);
  }

  // Record the dependency
  c->addInput(_versions.back());
}

void File::writtenBy(shared_ptr<Command> c) {
  // There must be a previous version if we're writing a file. If the first action performed on a
  // file is to write to it, there will be a create, reference, or truncate version already.
  FAIL_IF(_versions.size() == 0) << "Invalid write to file with no prior version: " << _path;

  // If the previous version was a write by this command, we don't need to tag a new version
  if (_versions.back()->_action == Version::Action::WRITE && _versions.back()->_writer == c) {
    return;
  }

  // Otherwise we tag a new written version
  auto v = Version::make_shared(Version::Action::WRITE, c);
  _versions.push_back(v);

  // Record the output edge
  c->addOutput(v);
}

void File::truncatedBy(shared_ptr<Command> c) {
  // Tag a truncated version
  auto v = Version::make_shared(Version::Action::TRUNCATE, c);
  _versions.push_back(v);

  // Record the output edge
  c->addOutput(v);
}

void File::deletedBy(shared_ptr<Command> c) {
  // Tag a deleted version
  auto v = Version::make_shared(Version::Action::DELETE, c);
  _versions.push_back(v);

  // Record the output edge
  c->addOutput(v);
}

void File::serialize(Serializer& serializer, db::File::Builder builder) {}
