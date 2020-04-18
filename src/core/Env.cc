#include "Env.hh"

#include <memory>

#include "core/Artifact.hh"
#include "core/IR.hh"

using std::dynamic_pointer_cast;
using std::shared_ptr;

// Check if an access resolves as-expected in the current environment
bool Env::checkAccess(shared_ptr<Reference> ref, int expected) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";
    // TODO: keep track of pipes in the environment, maybe?
    // Creating a pipe reference always succeeds. Check if SUCCESS was expected
    return expected == SUCCESS;

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // Look for the reference's path in the current environment
    // TODO: handle the nofollow flag
    // TODO: handle permissions
    // TODO: handle changes to directories along the path used by ref
    auto iter = _entries.find(a->getPath());
    if (iter != _entries.end()) {
      // TODO: currently-running command now depends on the command that created this entry
      return expected == SUCCESS;
    } else {
      // There was no entry in the environment. Check the actual filesystem
      return checkFilesystemAccess(a, expected);
    }

  } else {
    WARN << "Unsupported reference type: " << ref;
    return false;
  }
}

// Check if an access resolves as-expected in the filesystem
bool Env::checkFilesystemAccess(shared_ptr<Access> ref, int expected) {
  auto path = ref->getPath();
  auto flags = ref->getFlags();

  // If we hit this point, no match was found in any environment. Time to check the filesystem.
  // Set up an access mode that we'll check
  int access_mode = 0;
  if (flags.r) access_mode |= R_OK;
  if (flags.w) access_mode |= W_OK;
  if (flags.x) access_mode |= X_OK;

  // TODO: Is there anything to do for truncate? We need to be sure we can write the file, but is
  // it even possible to open with O_TRUNC in read-only mode?

  // Normally, faccessat checks whether the real user has access. We want to check as whatever the
  // effective user is. That's the same permission level the build would run with.
  int access_flags = AT_EACCESS;

  // Check access on a symlink if nofollow is specified
  if (flags.nofollow) access_flags |= AT_SYMLINK_NOFOLLOW;

  // Use faccessat to check the reference
  int rc = faccessat(AT_FDCWD, path.c_str(), access_mode, access_flags);

  // Check if the access() call failed for some reason
  if (rc) {
    // If the file does not exist, but O_CREAT was included, the access succeeds.
    // Does that match our expected outcome?
    // TODO: Check to be sure we have permission to create the file
    if (errno == ENOENT && flags.create) return expected == SUCCESS;

    // If we hit this point, it's a normal access and errno has the right code.
    // Does the errno value match our expected outcome?
    return expected == errno;
  } else {
    // If the file exists, but O_CREAT and O_EXCL were passed, fail
    if (flags.create && flags.exclusive) return expected == EEXIST;

    // Otherwise, the access succeeds. Does that match the expected outcome?
    return expected == SUCCESS;
  }
}

bool Env::checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    // TODO: Handle pipes correctly.
    // For now, we'll just say pipe metadata is always different (i.e. it does not match v)
    return false;

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // TODO: handle nofollow
    // Look for this reference in the current environment
    auto iter = _entries.find(a->getPath());
    if (iter != _entries.end()) {
      // Found a matching entry
      auto [command, current_version] = iter->second;

      // Does the current version in the environment match the expected version?
      return current_version == v;
    } else {
      // There is no matching entry in the environment. Check the actual filesystem
      return checkFilesystemMetadataMatch(a, v);
    }

  } else {
    WARN << "Unsupported reference type: " << ref;
    return false;
  }
}

bool Env::checkFilesystemMetadataMatch(shared_ptr<Access> ref, ArtifactVersion v) {
  // TODO: Pull code over from ArtifactVersion
  return v.metadataMatch(ref->getPath());
}

bool Env::checkContentsMatch(shared_ptr<Reference> ref, ArtifactVersion v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    // TODO: Handle pipes correctly.
    // For now, we'll just say pipe metadata is always different (i.e. it does not match v)
    return false;

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // TODO: handle nofollow
    // Look for this reference in the current environment
    auto iter = _entries.find(a->getPath());
    if (iter != _entries.end()) {
      // Found a matching entry
      auto [command, current_version] = iter->second;

      // Does the current version in the environment match the expected version?
      return current_version == v;
    } else {
      // There is no matching entry in the environment. Check the actual filesystem
      return checkFilesystemContentsMatch(a, v);
    }

  } else {
    WARN << "Unsupported reference type: " << ref;
    return false;
  }
}

bool Env::checkFilesystemContentsMatch(shared_ptr<Access> ref, ArtifactVersion v) {
  // TODO: move this over from ArtifactVersion
  return v.contentsMatch(ref->getPath());
}

void Env::setMetadata(shared_ptr<Reference> ref, ArtifactVersion v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // The path now resolves to this artifact version
    // TODO: Deal with links, path normalization, etc.
    _entries[a->getPath()] = {_commands.top(), v};

  } else {
    WARN << "Unsupported reference type: " << ref;
  }
}

void Env::setContents(shared_ptr<Reference> ref, ArtifactVersion v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // The path now resolves to this artifact version
    // TODO: Deal with links, path normalization, etc.
    _entries[a->getPath()] = {_commands.top(), v};

  } else {
    WARN << "Unsupported reference type: " << ref;
  }
}
