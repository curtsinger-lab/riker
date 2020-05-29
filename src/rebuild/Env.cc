#include "Env.hh"

#include <map>
#include <memory>
#include <string>
#include <tuple>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "data/Command.hh"
#include "data/IR.hh"
#include "data/Version.hh"
#include "rebuild/Artifact.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;
using std::tuple;

void Env::reset() {
  // TODO: could just roll back to artifacts and versions that exist pre-emulation, but for now
  // it's safe to just get rid of everything.
  _filesystem.clear();
  _files.clear();
  _pipes.clear();
}

tuple<shared_ptr<Artifact>, int, bool> Env::get(shared_ptr<Command> c, shared_ptr<Reference> ref) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    return getPipe(c, p);

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    return getFile(c, a);

  } else {
    WARN << "Unsupported reference type: " << ref;
    return {nullptr, ENOENT, false};
  }
}

tuple<shared_ptr<Artifact>, int, bool> Env::getPipe(shared_ptr<Command> c, shared_ptr<Pipe> ref) {
  // Check the _pipes map for an existing artifact
  auto iter = _pipes.find(ref);
  if (iter != _pipes.end()) {
    // Found a match. Return the artifact and result code
    return {iter->second, SUCCESS, false};

  } else {
    // No match found. Create a pipe artifact
    auto artifact = make_shared<Artifact>(ref);

    // Add the pipe to the map
    _pipes.emplace_hint(iter, ref, artifact);

    // Return the artifact and result code
    return {artifact, SUCCESS, true};
  }
}

tuple<shared_ptr<Artifact>, int, bool> Env::getFile(shared_ptr<Command> c, shared_ptr<Access> ref) {
  // Check the _files map to see if this reference has already been resolved
  if (auto iter = _files.find(ref); iter != _files.end()) {
    // Found a match. Return the artifact and result code
    return {iter->second, SUCCESS, false};
  }

  // At this point, we know this is a new reference. There are three possible outcomes:
  // 1. There is an artifact in the _filesystem map that matches this reference's path
  // 2. There is an on-disk file that we'll create an artifact to represent
  // 3. The resolution will fail with some error

  auto path = ref->getPath();
  auto flags = ref->getFlags();

  // First, look in the filesystem map
  // TODO: handle nofollow in the filesystem map
  if (auto iter = _filesystem.find(path); iter != _filesystem.end()) {
    // Found a match

    // If the access was required to create the file, return an error
    if (flags.create && flags.exclusive) {
      return {nullptr, EEXIST, false};
    }

    // Otherwise, the access will succeed. Save this in the map of resolved references
    _files.emplace(ref, iter->second);

    // Return the artifact and success code
    return {iter->second, SUCCESS, false};
  }

  // Use the access() system call to check the reference
  int rc = ref->access();

  // Check if the access() call failed for some reason
  if (rc) {
    // If the file does not exist, but O_CREAT was included, the access will succeed.
    // TODO: Check to be sure we have permission to create the file
    if (errno == ENOENT && flags.create) {
      // Create the artifact
      auto artifact = make_shared<Artifact>(ref);

      // Add this new artifact to the map of resolved references
      _files.emplace(ref, artifact);

      // Also add this new artifact to the filesystem map
      _filesystem.emplace(path, artifact);

      // And finally, return success
      return {artifact, SUCCESS, true};
    }

    // If we hit this point, it's a normal access and errno has the right code.
    return {nullptr, errno, false};

  } else {
    // If the file exists, but O_CREAT and O_EXCL were passed, the reference will fail
    if (flags.create && flags.exclusive) {
      return {nullptr, EEXIST, false};
    }

    // Otherwise, the access succeeds. Create an artifact to track this real file.
    auto artifact = make_shared<Artifact>(ref);

    // Because this artifact is on disk, create an initial version to track it
    artifact->createInitialVersion();

    // Save metadata and fingerprint for the initial version of the on-disk artifact
    artifact->getLatestVersion()->saveMetadata(ref);
    artifact->getLatestVersion()->saveFingerprint(ref);

    // Add this new artifact to the map of resolved references
    _files.emplace(ref, artifact);

    // Also add this new artifact to the filesystem map
    _filesystem.emplace(path, artifact);

    // And finally, return success
    return {artifact, SUCCESS, false};
  }
}
