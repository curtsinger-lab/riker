#include "Env.hh"

#include <memory>

#include "core/Artifact.hh"
#include "core/IR.hh"

using std::dynamic_pointer_cast;
using std::shared_ptr;

// Check if an access resolves as-expected in the current environment
bool CommandEnv::checkAccess(shared_ptr<Reference> ref, int expected) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Reference::Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";
    // TODO: keep track of pipes in the environment, maybe?
    // Creating a pipe reference always succeeds. Check if SUCCESS was expected
    return expected == SUCCESS;

  } else if (auto a = dynamic_pointer_cast<Reference::Access>(ref)) {
    // Look for the reference's path in the current environment
    // TODO: handle the nofollow flag
    // TODO: handle permissions
    if (_entries.find(a->getPath()) != _entries.end()) {
      return expected == SUCCESS;
    } else {
      return _link->checkAccess(ref, expected);
    }

  } else {
    FAIL << "Unsupported reference type";
  }
}

// Check if an access resolves as-expected in the filesystem
bool BaseEnv::checkAccess(shared_ptr<Reference> ref, int expected) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Reference::Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";
    return expected == SUCCESS;

  } else if (auto a = dynamic_pointer_cast<Reference::Access>(ref)) {
    auto path = a->getPath();
    auto flags = a->getFlags();

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
  } else {
    FAIL << "Unsupported reference type";
  }
}

bool CommandEnv::checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Reference::Pipe>(ref)) {
    // TODO: Handle pipes correctly.
    // For now, we'll just say pipe metadata is always different (i.e. it does not match v)
    return false;

  } else if (auto a = dynamic_pointer_cast<Reference::Access>(ref)) {
    // TODO: handle nofollow
    // Look for this reference in the current environment
    auto iter = _entries.find(a->getPath());
    if (iter == _entries.end()) {
      // Not here. Check the next environment
      return _link->checkMetadataMatch(path, v);
    } else {
      // Find it. Compare the versions.
      return iter->second == v;
    }

  } else {
    FAIL << "Unsupported reference type";
  }
}

bool BaseEnv::checkMetadataMatch(shared_ptr<Reference> ref, ArtifactVersion v) {
  // TODO: move this over from ArtifactVersion
  return v.metadataMatch(path);
}

bool CommandEnv::checkContentsMatch(string path, ArtifactVersion v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Reference::Pipe>(ref)) {
    // TODO: Handle pipes correctly.
    // For now, we'll just say pipe metadata is always different (i.e. it does not match v)
    return false;

  } else if (auto a = dynamic_pointer_cast<Reference::Access>(ref)) {
    // TODO: handle nofollow
    // Look for this reference in the current environment
    auto iter = _entries.find(a->getPath());
    if (iter == _entries.end()) {
      // Not here. Check the next environment
      return _link->checkContentsMatch(path, v);
    } else {
      // Find it. Compare the versions.
      return iter->second == v;
    }

  } else {
    FAIL << "Unsupported reference type";
  }
}

bool BaseEnv::checkContentsMatch(string path, ArtifactVersion v) {
  // TODO: move this over from ArtifactVersion
  return v.contentsMatch(path);
}

void CommandEnv::setMetadata(string path, ArtifactVersion v) {
  // The path now resolves to this artifact version
  // TODO: Deal with links, path normalization, etc.
  _entries[path] = v;
}

void CommandEnv::setContents(string path, ArtifactVersion v) {
  // The path now resolves to this artifact version
  // TODO: Deal with links, path normalization, etc.
  _entries[path] = v;
}
