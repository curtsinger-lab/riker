#include "Artifact.hh"

#include <memory>
#include <ostream>
#include <string>

#include <fcntl.h>
#include <sys/stat.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

using std::make_shared;
using std::ostream;
using std::shared_ptr;

size_t Artifact::next_id = 0;

ostream& operator<<(ostream& o, const Artifact* f) {
  string type = "Artifact";

  if (f->getType() == Artifact::Type::PIPE) {
    type = "Pipe";
  } else if (f->getType() == Artifact::Type::DIRECTORY) {
    type = "Dir";
  }

  o << "[" << type;
  if (f->getPath() != "") o << " " << f->getPath();
  o << "]";
  return o;
}

ostream& operator<<(ostream& o, const Artifact::Version* v) {
  return o << v->getArtifact() << "@" << v->getIndex();
}

void Artifact::createdBy(shared_ptr<Command> c) {
  // Tag a new created version
  auto v = makeVersion(Version::Action::CREATE, c);

  // Record the output edge from the command
  if (c->addOutput(v)) INFO << v << " created by " << c;
}

void Artifact::readBy(shared_ptr<Command> c) {
  // If this artifact has no previous versions, tag a version that references an existing artifact
  if (_versions.size() == 0) {
    // A reference version has no creator
    makeVersion(Version::Action::REFERENCE);
  }

  // Create a dependency if the latest version was not written by the same command that's reading
  auto latest = &_versions.back();
  if (latest->getAction() == Version::Action::REFERENCE || latest->getWriter() != c) {
    // Record the dependency
    if (c->addInput(&_versions.back())) INFO << c << " read " << &_versions.back();
  }
}

void Artifact::mayWrite(shared_ptr<Command> c) {
  // TODO
}

void Artifact::writtenBy(shared_ptr<Command> c) {
  // If this artifact has previous versions, we may not need to tag a new version
  if (_versions.size() > 0) {
    // Peek at the most recent version of the artifact
    Artifact::Version* prev_version = &_versions.back();

    // If the previous version was a write by this command, we don't need to tag a new version
    if (prev_version->_action == Version::Action::WRITE && prev_version->_writer == c) {
      return;
    }
  }

  // The state of this artifact depends on its previous version, so we create a read edge from the
  // previous version to the writing command. This is true even if the command cannot read the
  // artifact.
  readBy(c);

  // Now tag a new version written by this command
  auto v = makeVersion(Version::Action::WRITE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " wrote " << v;
}

void Artifact::mayTruncate(shared_ptr<Command> c) {
  // TODO
}

void Artifact::truncatedBy(shared_ptr<Command> c) {
  // Tag a truncated version
  auto v = makeVersion(Version::Action::TRUNCATE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " truncated " << v;
}

void Artifact::mayDelete(shared_ptr<Command> c) {
  // TODO
}

void Artifact::deletedBy(shared_ptr<Command> c) {
  // Tag a deleted version
  auto v = makeVersion(Version::Action::DELETE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " deleted " << v;
}

void Artifact::mayMap(shared_ptr<Command> c, bool writable) {
  // TODO
}

void Artifact::mappedBy(shared_ptr<Command> c, bool writable) {
  if (writable) {
    writtenBy(c);
    _writable_mappers.insert(c);
  } else {
    readBy(c);
    _read_only_mappers.insert(c);
  }
}

void Artifact::unmappedBy(shared_ptr<Command> c, bool writable) {
  if (writable) {
    _writable_mappers.erase(c);
  } else {
    _read_only_mappers.erase(c);
  }
}

Artifact::Version* Artifact::makeVersion(Version::Action a, shared_ptr<Command> c) {
  if (_versions.size() > 0) _versions.back().fingerprint();
  _versions.push_back(Version(this, _versions.size(), a, c));

  if (_versions.size() == 1) {
    _versions.back().fingerprint();
    if (_versions.back()._has_metadata && _type == Type::UNKNOWN) {
      switch (_versions.back()._metadata.st_mode & S_IFMT) {
        case S_IFDIR:
          _type = Type::DIRECTORY;
          break;

        case S_IFIFO:
          _type = Type::PIPE;
          break;

        case S_IFLNK:
          _type = Type::SYMLINK;
          break;

        default:
          _type = Type::REGULAR;
          break;
      }
    }
  }

  return &_versions.back();
}

void Artifact::Version::fingerprint() {
  if (_has_fingerprint) return;
  if (_artifact->getType() == Artifact::Type::PIPE) return;

  if (stat(_artifact->getPath().c_str(), &_metadata) == 0) {
    _has_metadata = true;
  } else {
    WARN << "Unable to stat artifact " << _artifact->getPath();
  }
}
