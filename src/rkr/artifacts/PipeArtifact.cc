#include "PipeArtifact.hh"

#include <memory>
#include <tuple>

#include <fcntl.h>
#include <unistd.h>

#include "data/AccessFlags.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/PipeVersion.hh"

using std::make_shared;
using std::shared_ptr;
using std::tuple;

/// Revert this artifact to its committed state
void PipeArtifact::rollback() noexcept {
  _last_reader.reset();
  _last_read.reset();
  _writes.clear();
  _committed_mode.reset();

  Artifact::rollback();
}

// Does this artifact have any uncommitted content?
bool PipeArtifact::hasUncommittedContent() noexcept {
  if (_committed_mode.has_value()) {
    return !_committed_mode.value();
  } else {
    return false;
  }
}

/// Commit a link to this artifact at the given path
void PipeArtifact::commitLink(shared_ptr<DirEntry> entry) noexcept {
  WARN << "Unimplemented PipeArtifact::commitLink()";
}

/// Commit an unlink of this artifact at the given path
void PipeArtifact::commitUnlink(shared_ptr<DirEntry> entry) noexcept {
  WARN << "Unimplemented PipeArtifact::commitUnlink()";
}

// A traced command is about to close a reference to this artifact
void PipeArtifact::beforeClose(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Is the command closing the last writable reference to this pipe?
  if (c->getRef(ref)->getFlags().w) {
    auto final_write = make_shared<PipeCloseVersion>();

    // Intentionally not calling build.traceUpdateContent here. That will implicitly be invoked when
    // the final reference to this pipe is closed.
    updateContent(c, final_write);
  }
}

// A traced command just read from this artifact
void PipeArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Make sure there are no uncommitted updates
  if (!_committed_mode.value_or(true)) {
    WARN << "Traced command " << c << " is reading from " << this << " with uncommitted state";

    if (_last_read) {
      WARN << "  Last read: " << _last_read << " by " << _last_reader.lock();
    } else {
      WARN << "  No previous read from pipe";
    }

    WARN << "  Writes:";
    for (auto& [v, weak_writer] : _writes) {
      WARN << "    " << v << " by " << weak_writer.lock();
    }

    FAIL << "Stopping";
  }

  // The reading command depends on the last read
  if (_last_read) {
    c->addContentInput(shared_from_this(), _last_read, _last_reader.lock());
  }

  // Create a new version to track this read
  auto read_version = make_shared<PipeReadVersion>();

  LOG(artifact) << "Creating pipe read version " << read_version;

  // The reader updates this pipe with the read version
  build.updateContent(c, ref, read_version);

  // The command should expect to read an equivalent version on future builds
  build.matchContent(c, Scenario::Build, ref, read_version);
}

// A trace command just wrote to this artifact
void PipeArtifact::beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Create a new version
  auto writing = make_shared<PipeWriteVersion>();

  // The command writes this version to the pipe
  build.updateContent(c, ref, writing);
}

// Get this artifact's current content
shared_ptr<ContentVersion> PipeArtifact::getContent(const shared_ptr<Command>& c) noexcept {
  if (_last_read) {
    if (c) {
      c->addContentInput(shared_from_this(), _last_read, _last_reader.lock());
    }
    return _last_read;
  } else {
    if (c) {
      for (const auto& [write, writer] : _writes) {
        c->addContentInput(shared_from_this(), write, writer.lock());
      }
    }
    return make_shared<PipeReadVersion>();
  }
}

// Check to see if this artifact's content matches a known version
void PipeArtifact::matchContent(const shared_ptr<Command>& c,
                                Scenario scenario,
                                shared_ptr<ContentVersion> expected) noexcept {
  // If nothing has been read from this pipe, there can be no match
  if (!_last_read) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _last_read);
    c->inputChanged(shared_from_this(), nullptr, expected, scenario);
    return;
  }

  // The command depends on the last read version
  c->addContentInput(shared_from_this(), _last_read, _last_reader.lock());

  // Compare the current content version to the expected version
  if (!_last_read->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _last_read);
    // Report the mismatch
    c->inputChanged(shared_from_this(), _last_read, expected, scenario);
  }
}

// Apply a new content version to this artifact
void PipeArtifact::updateContent(const shared_ptr<Command>& c,
                                 shared_ptr<ContentVersion> writing) noexcept {
  // Append the new version to the list of versions
  appendVersion(writing);

  // Has this pipe been assigned a committed/uncommitted mode?
  if (_committed_mode.has_value()) {
    if (_committed_mode.value()) {
      ASSERT(c->mustRun()) << this << " (" << (void*)this << ") is in committed mode, but " << c
                           << " is not running and wrote version " << writing;
    } else {
      ASSERT(!c->mustRun()) << this << " (" << (void*)this << ") is in uncommitted mode, but " << c
                            << " is running and wrote version " << writing;
    }
  } else {
    // No. Assign the mode now.
    _committed_mode = c->mustRun();
  }

  // Report the output to the build
  c->addContentOutput(shared_from_this(), writing);

  // Is the written version a pipe write or pipe read?
  if (auto read = writing->as<PipeReadVersion>()) {
    // Set the last read version and clear the list of writes
    _last_read = read;

    // The reading command depends on all writes since the last read
    for (const auto& [write, writer] : _writes) {
      c->addContentInput(shared_from_this(), write, writer.lock());
    }

    // Clear the list of unread writes
    _writes.clear();

  } else if (auto close = writing->as<PipeCloseVersion>()) {
    // Add the close operation to the list of un-read writes
    _writes.emplace_back(close, c);

  } else if (auto write = writing->as<PipeWriteVersion>()) {
    // Add this write to the list of un-read writes
    _writes.emplace_back(write, c);

  } else {
    FAIL << "Unsupported pipe version type " << writing;
  }
}

int PipeArtifact::getFD(AccessFlags flags) noexcept {
  ASSERT((flags.r && !flags.w) || (flags.w && !flags.r))
      << "Invalid access flags for pipe: " << flags;

  if (!_fds.has_value()) {
    int pipefds[2];
    int rc = pipe2(pipefds, O_CLOEXEC);
    FAIL_IF(rc != 0) << "Failed to create pipe";
    _fds = tuple{pipefds[0], pipefds[1]};
    LOG(artifact) << "Created pipe for " << this << " with read fd " << pipefds[0]
                  << " and write fd " << pipefds[1];
  }

  auto [read_fd, write_fd] = _fds.value();

  if (flags.r) {
    ASSERT(read_fd >= 0) << "Attempted to return invalid pipe file descriptor " << read_fd
                         << " from read end of " << this;
    // Mark the read fd as invalid so it cannot be returned a second time
    _fds = tuple{-1, write_fd};
    return read_fd;

  } else {
    ASSERT(write_fd >= 0) << "Attempted to return invalid pipe file descriptor " << write_fd
                          << " from write end of " << this;
    // Mark teh write fd as invalid so it cannot be returned a second time
    _fds = tuple{read_fd, -1};
    return write_fd;
  }
}
