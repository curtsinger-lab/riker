#include "Artifact.hh"

#include <memory>

#include <fcntl.h>
#include <sys/stat.h>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "ui/log.hh"

using std::shared_ptr;

size_t Artifact::next_id = 0;

const shared_ptr<Artifact> Artifact::stdin = make_shared<Artifact>("stdin", Artifact::Type::PIPE);
const shared_ptr<Artifact> Artifact::stdout = make_shared<Artifact>("stdout", Artifact::Type::PIPE);
const shared_ptr<Artifact> Artifact::stderr = make_shared<Artifact>("stderr", Artifact::Type::PIPE);

void Artifact::createdBy(shared_ptr<Command> c) {
  // Tag a new created version
  auto v = makeVersion(Action::CREATE, c);

  // Record the output edge from the command
  if (c->addOutput(v)) INFO << v << " created by " << c;
}

void Artifact::readBy(shared_ptr<Command> c) {
  // If this artifact has no previous versions, tag a version that references an existing artifact
  if (_versions.size() == 0) {
    // A reference version has no creator
    makeVersion(Action::REFERENCE);
  }

  // Create a dependency if the latest version was not written by the same command that's reading
  auto latest = getLatestVersion();
  if (latest.getAction() == Action::REFERENCE || latest.getWriter() != c) {
    // Record the dependency
    if (c->addInput(latest)) INFO << c << " read " << latest;
  }
}

void Artifact::mayWrite(shared_ptr<Command> c) {
  // TODO
}

void Artifact::writtenBy(shared_ptr<Command> c) {
  // If this artifact has previous versions, we may not need to tag a new version
  if (_versions.size() > 0) {
    // Peek at the most recent version of the artifact
    auto latest = getLatestVersion();

    // If the previous version was a write by this command, we don't need to tag a new version
    if (latest.getAction() == Action::WRITE && latest.getWriter() == c) {
      return;
    }
  }

  // The state of this artifact depends on its previous version, so we create a read edge from the
  // previous version to the writing command. This is true even if the command cannot read the
  // artifact.
  readBy(c);

  // Now tag a new version written by this command
  auto v = makeVersion(Action::WRITE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " wrote " << v;
}

void Artifact::mayTruncate(shared_ptr<Command> c) {
  // TODO
}

void Artifact::truncatedBy(shared_ptr<Command> c) {
  // Tag a truncated version
  auto v = makeVersion(Action::TRUNCATE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " truncated " << v;
}

void Artifact::mayDelete(shared_ptr<Command> c) {
  // TODO
}

void Artifact::deletedBy(shared_ptr<Command> c) {
  // Tag a deleted version
  auto v = makeVersion(Action::DELETE, c);

  // Record the output edge
  if (c->addOutput(v)) INFO << c << " deleted " << v;
}

void Artifact::mappedBy(shared_ptr<Command> c, bool writable) {
  // We're going to insert this command into the reader or writer mapper list
  auto& mapper_list = writable ? _writable_mappers : _read_only_mappers;

  // Record a read or write
  if (writable)
    writtenBy(c);
  else
    readBy(c);

  // Now scan the mapper list to see if this command is already in it
  for (auto d : mapper_list) {
    // Promote the weak pointer to a shared_ptr and check if it matches the new mapper command
    // If we find a match, this command is already in the list so we can return
    if (d.lock() == c) return;
  }

  // If we hit this point, the command c is a new mapper. Add it to the list
  mapper_list.push_back(c);
}

void Artifact::unmappedBy(shared_ptr<Command> c, bool writable) {
  // We'll remove this command from either the writable or read-only mapper list
  auto& mapper_list = writable ? _writable_mappers : _read_only_mappers;

  // Loop through the list to find a match
  for (auto iter = mapper_list.begin(); iter != mapper_list.end(); iter++) {
    // If we find a match, remove the command from the list and return
    if (iter->lock() == c) {
      mapper_list.erase(iter);
      return;
    }
  }
}

Artifact::VersionRef Artifact::makeVersion(Action a, shared_ptr<Command> c) {
  // Take a fingerprint before creating a new version
  fingerprint();

  _versions.push_back(Version(a, c));

  if (_versions.size() == 1) {
    if (_versions.back().has_metadata && _type == Type::UNKNOWN) {
      switch (_versions.back().metadata.st_mode & S_IFMT) {
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

  return VersionRef(shared_from_this(), _versions.size() - 1);
}

void Artifact::fingerprint() {
  // If there are no references to this file, there's no need to fingerprint
  if (_versions.size() == 0) return;

  // Get the record for the latest version
  Version& r = _versions.back();

  // If we already have a fingerprint, bail out
  if (r.has_fingerprint) return;

  // We can't fingerprint pipes
  if (_type == Artifact::Type::PIPE) return;

  // Stat the file
  if (stat(_path.c_str(), &r.metadata) == 0) {
    r.has_metadata = true;
  } else {
    WARN << "Unable to stat artifact " << _path;
  }
}
