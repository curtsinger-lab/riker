#include "Rebuild.hh"

#include <cerrno>
#include <ctime>
#include <iostream>
#include <memory>
#include <ostream>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "data/Command.hh"
#include "data/IR.hh"
#include "data/Version.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::make_shared;
using std::ostream;

// Create a rebuild plan
Rebuild Rebuild::create(shared_ptr<Command> root) {
  // Initialize the rebuild with the build's root command
  Rebuild r(root);

  // Identify commands with changed dependencies
  r.findChanges(root);

  // Check the final outputs of the emulated build against the filesystem
  r.checkFinalState();

  // Mark all the commands with changed inputs
  for (auto& c : r._changed) {
    r.mark(c);
  }

  // Mark all the commands whose output is required
  for (auto& c : r._output_needed) {
    r.mark(c);
  }

  return r;
}

// Run a rebuild, updating the in-memory build representation
void Rebuild::run() {
  // Create a tracing context to run the build
  Tracer tracer(*this);

  // Run or emulate the root command with the tracer
  runCommand(_root, tracer);

  // Finish up by saving metadata for any remaining artifacts
  for (auto& [_, entry] : _artifacts) {
    auto& [path, artifact] = entry;
    auto ref = make_shared<Access>(path, AccessFlags());
    artifact->getLatestVersion()->saveMetadata(ref);
    artifact->getLatestVersion()->saveFingerprint(ref);
  }
}

// Run or emulate a command in this rebuild
void Rebuild::runCommand(shared_ptr<Command> c, Tracer& tracer) {
  // Does the rebuild plan say command c must run?
  if (mustRerun(c)) {
    // We are rerunning this command, so clear the lists of steps and children
    c->reset();

    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) cout << c->getFullName() << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) tracer.run(c);

  } else {
    // Emulate this command by running its children
    for (auto& child : c->getChildren()) {
      runCommand(child, tracer);
    }
  }
}

// Get an artifact during tracing
shared_ptr<Artifact> Rebuild::getArtifact(shared_ptr<Reference> ref) {
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    // TODO: every pipe ref creates a new artifact for now. We'll need to track them eventually.
    return make_shared<Artifact>("PIPE");

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    string p = a->getPath();
    bool follow_links = !a->getFlags().nofollow;

    // Now that we have a path, we can stat it
    struct stat statbuf;
    int rc;
    if (follow_links)
      rc = stat(p.c_str(), &statbuf);
    else
      rc = lstat(p.c_str(), &statbuf);

    // If stat failed, there is no artifact to resolve to. Return a null pointer
    if (rc) return shared_ptr<Artifact>();

    // Check for an existing inode entry
    auto iter = _artifacts.find(statbuf.st_ino);
    if (iter != _artifacts.end()) {
      // Found. Return it.
      return iter->second.second;
    }

    // No existing artifact found. Create a new one.
    shared_ptr<Artifact> result = make_shared<Artifact>(p);

    // Add the artifact to the map
    _artifacts.emplace(statbuf.st_ino, pair<string, shared_ptr<Artifact>>(p, result));

    // All done
    return result;
  } else {
    FAIL << "Unsupported reference type";
    return nullptr;  // Unreachable
  }
}

// Show rebuild information
ostream& Rebuild::print(ostream& o) const {
  if (_changed.size() > 0) {
    o << "Commands with changed inputs:" << endl;
    for (auto& c : _changed) {
      o << "  " << c << endl;
    }
    o << endl;
  }

  if (_output_needed.size() > 0) {
    o << "Commands whose output is missing or modified:" << endl;
    for (auto& c : _output_needed) {
      o << "  " << c << endl;
    }
    o << endl;
  }

  if (_rerun.size() > 0) {
    o << "A rebuild will run the following commands:" << endl;
    for (auto& c : _rerun) {
      o << "  " << c << endl;
    }
  } else {
    o << "No changes detected" << endl;
  }

  return o;
}

void Rebuild::findChanges(shared_ptr<Command> c) {
  // Keep track of whether we've seen any changes for command c
  bool changed = false;

  // If this command has never run, it is definitely changed
  if (c->neverRun()) {
    LOG << c << " changed: never run";
    changed = true;
  }

  // Loop over the steps from the command trace to see if command c will see any changes
  for (auto step : c->getSteps()) {
    // Handle each IR type here
    if (auto pipe = dynamic_pointer_cast<Pipe>(step)) {
      // Nothing to do for pipes

    } else if (auto access = dynamic_pointer_cast<Access>(step)) {
      // Nothing to do for access

    } else if (auto ref_result = dynamic_pointer_cast<ReferenceResult>(step)) {
      // Check if the reference resolves the same way
      if (!checkAccess(c, ref_result->getReference(), ref_result->getResult())) {
        LOG << c << " changed: " << ref_result;
        changed = true;
      }

    } else if (auto m_match = dynamic_pointer_cast<MetadataMatch>(step)) {
      // Check if the metadata in the environment matches the expected version
      if (!checkMetadata(c, m_match->getReference(), m_match->getVersion())) {
        LOG << c << " changed: " << m_match;
        changed = true;
      }

    } else if (auto c_match = dynamic_pointer_cast<ContentsMatch>(step)) {
      // Check if the contents in the environment match the expected version
      if (!checkContents(c, c_match->getReference(), c_match->getVersion())) {
        LOG << c << " changed: " << c_match;
        changed = true;
      }

    } else if (auto launch = dynamic_pointer_cast<Launch>(step)) {
      // Check the child command's inputs
      findChanges(launch->getCommand());

    } else if (auto set_metadata = dynamic_pointer_cast<SetMetadata>(step)) {
      // Update the metadata in the environment
      setMetadata(c, set_metadata->getReference(), set_metadata->getVersion());

    } else if (auto set_contents = dynamic_pointer_cast<SetContents>(step)) {
      // Update the contents in the environment
      setContents(c, set_contents->getReference(), set_contents->getVersion());

    } else {
      FAIL << "Unsupported IR action " << step;
    }
  }

  // If anything was different, add c to the set of commands with changed inputs
  if (changed) _changed.insert(c);
}

void Rebuild::checkFinalState() {
  // Loop over all entries in the environment
  for (auto& [path, entry] : _entries) {
    // Create a fake reference to check the file, just for now
    auto ref = make_shared<Access>(path, AccessFlags());

    // Check the filesystem to see if the real file matches our expected version
    if (!checkFilesystemContents(ref, entry)) {
      _output_needed.insert(entry->getCreator());
    }
  }
}

void Rebuild::mark(shared_ptr<Command> c) {
  // If command c was already marked, return
  if (_rerun.find(c) != _rerun.end()) return;

  // Otherwise, we have work to do. Frist, add command c to the rerun set.
  _rerun.insert(c);

  // Rerunning this command also reruns its children
  for (auto& child : c->getChildren()) {
    mark(child);
  }

  // If command c requires output from other commands, mark them
  for (auto& needed : _needs_output_from[c]) {
    mark(needed);
  }

  // If other commands depend on output from command c, mark them
  for (auto& other : _output_used_by[c]) {
    mark(other);
  }
}

// Check if an access resolves as-expected in the current environment
bool Rebuild::checkAccess(shared_ptr<Command> c, shared_ptr<Reference> ref, int expected) {
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
      // Get the writing command and the entry it wrote
      auto [path, entry] = *iter;

      // If the writer reruns, the current command will need to rerun too because it depends on
      // writer's output.
      _output_used_by[entry->getCreator()].insert(c);

      // If we had a cached version of the entry writer creates we could skip this, but no caching
      // yet so any time we need to rerun the current command, writer will have to rerun first.
      _needs_output_from[c].insert(entry->getCreator());

      // This access will succeed, so check if that matches the expected outcome
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

bool Rebuild::checkMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref,
                            shared_ptr<Version> v) {
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
      auto [path, entry] = *iter;

      // If the writer ever reruns, the current command must rerun as well. Record that.
      _output_used_by[entry->getCreator()].insert(c);

      // This command also requires output from the writer. If we have the metadata cached, we don't
      // necessarily have to run the command that sets it, since we can put it in place ourselves.
      if (!entry->hasMetadata()) {
        _needs_output_from[c].insert(entry->getCreator());
      } else {
        // TODO: This may be the place to record that we have to stage in the expected artifact
        // version if this command is run and the writer is not.
      }

      // Does the current version in the environment match the expected version?
      return entry == v;
    } else {
      // There is no matching entry in the environment. Check the actual filesystem
      return checkFilesystemMetadata(a, v);
    }

  } else {
    WARN << "Unsupported reference type: " << ref;
    return false;
  }
}

bool Rebuild::checkContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                            shared_ptr<Version> v) {
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
      auto [path, entry] = *iter;

      // If the writer ever reruns, the current command must rerun as well. Record that.
      _output_used_by[entry->getCreator()].insert(c);

      // This command also requires output from the writer
      // If we have file saved we can stage it in instead of running the writing command.
      if (!entry->hasSavedContents()) {
        _needs_output_from[c].insert(entry->getCreator());
      } else {
        // TODO: This may be the place to record that we have to stage in the expected artifact
        // version if this command is run and the writer is not.
      }

      // Does the current version in the environment match the expected version?
      return entry == v;
    } else {
      // There is no matching entry in the environment. Check the actual filesystem
      return checkFilesystemContents(a, v);
    }

  } else {
    WARN << "Unsupported reference type: " << ref;
    return false;
  }
}

void Rebuild::setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref, shared_ptr<Version> v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // The path now resolves to this artifact version
    // TODO: Deal with links, path normalization, etc.
    _entries[a->getPath()] = v;

  } else {
    WARN << "Unsupported reference type: " << ref;
  }
}

void Rebuild::setContents(shared_ptr<Command> c, shared_ptr<Reference> ref, shared_ptr<Version> v) {
  // Is ref a pipe, access, or something else?
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    WARN << "Warning: Communication through pipes is not yet tracked correctly.";

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    // The path now resolves to this artifact version
    // TODO: Deal with links, path normalization, etc.
    _entries[a->getPath()] = v;

  } else {
    WARN << "Unsupported reference type: " << ref;
  }
}

// Check if an access resolves as-expected in the filesystem
bool Rebuild::checkFilesystemAccess(shared_ptr<Access> ref, int expected) {
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

/// Equality function for timespec structs
bool operator==(const timespec& t1, const timespec& t2) {
  return t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec;
}

/// Non-equality operator for timespec structs
bool operator!=(const timespec& t1, const timespec& t2) {
  return !(t1 == t2);
}

/// Printing for timespec structs
ostream& operator<<(ostream& o, const timespec& ts) {
  return o << ts.tv_sec << ":" << ts.tv_nsec;
}

// Check if the metadata for a file on the actual filesystem matches a saved version
bool Rebuild::checkFilesystemMetadata(shared_ptr<Access> ref, shared_ptr<Version> v) {
  // If we don't have metadata saved, we have to assume the file has changed
  if (!v->hasMetadata()) return false;

  // TODO: handle nofollow!

  // Try to stat. If the stat fails, metadata does not match
  struct stat metadata;
  if (stat(ref->getPath().c_str(), &metadata) != 0) return false;

  auto saved_metadata = v->getMetadata();

  // We only compare uid, gid, and mode (which covers both type and permissions)
  if (metadata.st_uid != saved_metadata.st_uid) {
    LOG << "uid mismatch";
    return false;
  }

  if (metadata.st_gid != saved_metadata.st_gid) {
    LOG << "gid mismatch";
    return false;
  }

  if (metadata.st_mode != saved_metadata.st_mode) {
    LOG << "mode mismatch";
    return false;
  }

  // That's it. Metadata must match
  return true;
}

// Check if the contents of a file on the actual fileystem match a saved version
bool Rebuild::checkFilesystemContents(shared_ptr<Access> ref, shared_ptr<Version> v) {
  // For now, we're just going to check mtime

  // If we don't have metadata saved, we have to assume the file has changed
  if (!v->hasMetadata()) return false;

  // TODO: handle nofollow!

  // Try to stat. If the stat fails, metadata does not match
  struct stat metadata;
  if (stat(ref->getPath().c_str(), &metadata) != 0) return false;

  auto saved_metadata = v->getMetadata();

  // If the mtime for the on-disk file is changed, the contents must not match
  if (metadata.st_mtim != saved_metadata.st_mtim) {
    LOG << "mtime changed: " << metadata.st_mtim << " vs " << saved_metadata.st_mtim;
    return false;
  }

  // That's it for now. If mtime is unchanged, the file must be unchanged
  return true;
}
