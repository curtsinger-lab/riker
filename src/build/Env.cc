#include "Env.hh"

#include <map>
#include <memory>
#include <string>
#include <tuple>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "artifacts/Dir.hh"
#include "artifacts/File.hh"
#include "artifacts/Pipe.hh"
#include "build/Build.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;
using std::tuple;

tuple<shared_ptr<Artifact>, int> Env::get(shared_ptr<Command> c,
                                          shared_ptr<Reference> ref) noexcept {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    return getPipe(c, p);

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    return getFile(c, a);

  } else {
    WARN << "Unsupported reference type: " << ref;
    return {nullptr, ENOENT};
  }
}

tuple<shared_ptr<Artifact>, int> Env::getPipe(shared_ptr<Command> c,
                                              shared_ptr<Pipe> ref) noexcept {
  // Check the _pipes map for an existing artifact
  auto iter = _pipes.find(ref);
  if (iter != _pipes.end()) {
    // Found a match. Return the artifact and result code
    return {iter->second, SUCCESS};

  } else {
    // No match found. Create a pipe artifact
    auto artifact = make_shared<PipeArtifact>(*this, true);

    // Add the pipe to the map
    _pipes.emplace_hint(iter, ref, artifact);

    // Return the artifact and result code
    return {artifact, SUCCESS};
  }
}

tuple<shared_ptr<Artifact>, int> Env::getFile(shared_ptr<Command> c,
                                              shared_ptr<Access> ref) noexcept {
  // Check the _files map to see if this reference has already been resolved
  if (auto iter = _files.find(ref); iter != _files.end()) {
    // Found a match. Return the artifact and result code
    return {iter->second, SUCCESS};
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
      return {nullptr, EEXIST};
    }

    // Otherwise, the access will succeed. Save this in the map of resolved references
    _files.emplace(ref, iter->second);

    // Return the artifact and success code
    return {iter->second, SUCCESS};
  }

  // Use the access() system call to check the reference
  int rc = ref->access();

  // Check if the access() call failed for some reason
  if (rc) {
    // If the file does not exist, but O_CREAT was included, the access will succeed.
    // TODO: Check to be sure we have permission to create the file
    if (errno == ENOENT && flags.create) {
      // We are going to create an artifact in the filesystem model, but we need to pre-load its
      // version with manufactured stat data.

      // Get the current umask
      auto mask = umask(0);
      umask(mask);

      // Create a stat buffer
      struct stat metadata;
      metadata.st_uid = geteuid();
      metadata.st_gid = getegid();
      metadata.st_mode = S_IFREG | (flags.mode & ~mask);

      // Create the initial versions for this artifact
      auto mv = make_shared<MetadataVersion>(metadata);
      auto cv = make_shared<ContentVersion>();

      // Create the artifact. Because it's being created, mark it as committed (even though it isn't
      // actually on-disk yet).
      // TODO: Add a just_created flag to this method so we don't have to do this hackery
      auto artifact = make_shared<FileArtifact>(*this, true, mv, cv);
      artifact->setName(path);

      // Add this new artifact to the map of resolved references
      _files.emplace(ref, artifact);

      // Also add this new artifact to the filesystem map
      _filesystem.emplace(path, artifact);

      // Notify the build that the creating command wrote to this artifact
      _build.observeOutput(c, artifact, mv);
      _build.observeOutput(c, artifact, cv);

      // And finally, return success
      return {artifact, SUCCESS};
    }

    // If we hit this point, it's a normal access and errno has the right code.
    return {nullptr, errno};

  } else {
    // If the file exists, but O_CREAT and O_EXCL were passed, the reference will fail
    if (flags.create && flags.exclusive) {
      return {nullptr, EEXIST};
    }

    // Otherwise, the access succeeds. We need to stat the path to find out if it's a file or
    // directory.
    auto [statbuf, rc] = ref->stat();
    ASSERT(rc == SUCCESS) << "Failed to stat successfully-accessed path";

    shared_ptr<Artifact> artifact;

    // Handle filesystem types
    if ((statbuf.st_mode & S_IFMT) == S_IFDIR) {
      // Directory

      // If the access flags include reading or writing, and a directory was not specifically
      // requested, then the access fails with an error
      if ((flags.r || flags.w) && !flags.directory) return {nullptr, EISDIR};

      // Have we seen this directory before?
      auto iter = _inodes.find(statbuf.st_ino);
      if (iter != _inodes.end()) {
        artifact = iter->second;
      } else {
        artifact = make_shared<DirArtifact>(*this, true, make_shared<MetadataVersion>(statbuf));
        _inodes.emplace_hint(iter, statbuf.st_ino, artifact);
      }

    } else if ((statbuf.st_mode & S_IFMT) == S_IFREG) {
      // File
      // If the access flags include reading or writing, and a directory was requested, this fails
      if (flags.directory) return {nullptr, ENOTDIR};

      // Have we seen this file before?
      auto iter = _inodes.find(statbuf.st_ino);
      if (iter != _inodes.end()) {
        artifact = iter->second;
      } else {
        artifact = make_shared<FileArtifact>(*this, true, make_shared<MetadataVersion>(statbuf),
                                             make_shared<ContentVersion>(statbuf));
        _inodes.emplace_hint(iter, statbuf.st_ino, artifact);
      }

    } else {
      // Someting else. Just make it a file for now
      WARN << "Unexpected filesystem node type at " << path << ". Treating it as a file.";

      // If the access flags include reading or writing, and a directory was requested, this fails
      if (flags.directory) return {nullptr, ENOTDIR};

      // Have we seen this inode before?
      auto iter = _inodes.find(statbuf.st_ino);
      if (iter != _inodes.end()) {
        artifact = iter->second;
      } else {
        artifact = make_shared<FileArtifact>(*this, true, make_shared<MetadataVersion>(statbuf),
                                             make_shared<ContentVersion>(statbuf));
        _inodes.emplace_hint(iter, statbuf.st_ino, artifact);
      }
    }

    // Name the artifact
    artifact->setName(path);

    // Add this new artifact to the map of resolved references
    _files.emplace(ref, artifact);

    // Also add this new artifact to the filesystem map
    _filesystem.emplace(path, artifact);

    // And finally, return success
    return {artifact, SUCCESS};
  }
}

// Check all remaining artifacts for changes and save updated fingerprints and metadata
void Env::finalize() noexcept {
  // Loop over all the artifacts
  for (auto& [ref, a] : _files) {
    // Check the artifact's final contents and metadata against the filesystem
    a->checkFinalState(ref);

    // Save fingerprint and metadata for this artifact
    a->fingerprint(ref);
  }
}
