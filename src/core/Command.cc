#include "Command.hh"

#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Build.hh"
#include "core/AccessFlags.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "util/path.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::cout;
using std::endl;
using std::make_shared;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

/**
 * This class is used to determine whether a read/write operation must be added to the trace, or if
 * it can safely be elided. Each type of access (metdata, contents, etc.) can use a separate filter
 * because two accesses cannot alias each other if they refer to different version types.
 *
 * The filter implements the following logic:
 * - Repeated reads by the same command through the same reference can be skipped
 * - Repeated writes by the same command through the same reference can be skipped, unless the
 *   previously-written version has been accessed
 * - Any new write serves as a barrier, so all reads after that write are considered new, except for
 *   reads by the writing command through the same reference
 *
 * This may seem overly-conservative, but it is necessary because any two references could alias on
 * some future rebuild. Therefore, any write to metadata could change the outcome of any future
 * read, so those reads after each write must be checked (and therefore must appear in the trace).
 */
class AccessFilter {
 public:
  // Record the effect of a read by command c using reference ref
  void read(Command* c, shared_ptr<Reference> ref) noexcept {
    // Command c can read through reference ref without logging until the next write
    _observed.emplace(c, ref.get());
  }

  // Record the effect of a write by command c using reference ref
  void write(Command* c, shared_ptr<Reference> ref, shared_ptr<Version> written) noexcept {
    // All future reads could be affected by this write, so they need to be logged
    _observed.clear();

    // Keep track of the last write
    _last_writer = c;
    _last_write_ref = ref.get();
    _last_written_version = written.get();

    // The writer can observe its own written value without logging
    _observed.emplace(c, ref.get());
  }

  // Does command c need to add a read through reference ref to its trace?
  bool readRequired(Command* c, shared_ptr<Reference> ref) noexcept {
    // If this optimization is disabled, the read is always required
    if (!options::combine_reads) return true;

    // If this command has already read through this reference, the read is not required
    if (_observed.find({c, ref.get()}) != _observed.end()) return false;

    // Otherwise the read is required
    return true;
  }

  // Does command c need to add a write through reference ref to its trace?
  bool writeRequired(Command* c, shared_ptr<Reference> ref) noexcept {
    // If this optimization is disabled, the write is always required
    if (!options::combine_writes) return true;

    // If this is the first write through the filter, it must be added to the trace
    if (!_last_written_version) return true;

    // If the last version written through this filter was accessed, add a new write to the trace
    if (_last_written_version->isAccessed()) return true;

    // If a different command is writing, add a new write to the trace
    if (c != _last_writer) return true;

    // If the same command is using a different reference to write, add a new write to the trace
    if (ref.get() != _last_write_ref) return true;

    // This write is by the same command as the last write using the same reference.
    // The previously-written value has not been accessed, so we do not need to log a new write.
    return false;
  }

 private:
  Command* _last_writer = nullptr;
  Reference* _last_write_ref = nullptr;
  Version* _last_written_version = nullptr;
  set<pair<Command*, Reference*>> _observed;
};

/// The access filter used for metadata accesses
static AccessFilter _metadata_filter;

/// The access filter used for content accesses
static AccessFilter _content_filter;

string Command::getShortName(size_t limit) const noexcept {
  // By default, the short name is the executable
  auto exe_path = _exe->getFullPath();

  // If we have arguments, use args[0] instead of the exe name
  if (_args.size() > 0) exe_path = _args.front();

  // If the exe_path is an absolute path, use the filename
  if (exe_path.is_absolute()) exe_path = exe_path.filename();

  // The output starts with the executable name
  string result = exe_path;

  // Add arguments up to the length limit
  size_t index = 1;
  while (index < _args.size() && result.length() < limit) {
    result += " " + _args[index];
    index++;
  }

  if (result.length() >= limit) {
    result = result.substr(0, limit - 3) + "...";
  }

  return result;
}

string Command::getFullName() const noexcept {
  string result;
  bool first = true;
  for (const string& arg : _args) {
    if (!first) result += " ";
    first = false;
    result += arg;
  }
  return result;
}

// This command accesses an artifact by path.
shared_ptr<Access> Command::access(Build& build,
                                   fs::path path,
                                   AccessFlags flags,
                                   shared_ptr<Access> base) noexcept {
  // For now, the reference is just one level that covers all parts of the new path
  auto ref = base->get(path, flags);
  build.addStep(shared_from_this(), ref);
  return ref;
}

// This command creates a reference to a new pipe
shared_ptr<Pipe> Command::pipe(Build& build) noexcept {
  auto ref = make_shared<Pipe>();
  build.addStep(shared_from_this(), ref);
  return ref;
}

// This command depends on the metadata of a referenced artifact
void Command::metadataMatch(Build& build, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a metadata match on an unresolved reference.";

  // Do we have to log this read?
  if (!_metadata_filter.readRequired(this, ref)) return;

  // Inform the artifact that this command accesses its metadata
  const auto& v = ref->getArtifact()->accessMetadata(shared_from_this(), ref, InputType::Accessed);

  // If the last write was from this command we don't need a fingerprint to compare.
  if (v->getCreator() != shared_from_this()) {
    v->fingerprint(ref);
  }

  // Add the IR step
  build.addStep(shared_from_this(), make_shared<MetadataMatch>(ref, v));

  // Report the read
  _metadata_filter.read(this, ref);
}

// This command depends on the contents of a referenced artifact
void Command::contentsMatch(Build& build, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a content match on an unresolved reference: "
                            << ref;

  // Do we have to log this read?
  if (!_content_filter.readRequired(this, ref)) return;

  // Inform the artifact that this command accesses its contents
  const auto& v = ref->getArtifact()->accessContents(shared_from_this(), ref);

  if (!v) {
    WARN << "Accessing contents of " << ref << " returned a null version";
    return;
  }

  // If the last write was from this command, we don't need a fingerprint to compare.
  if (v->getCreator() != shared_from_this()) {
    v->fingerprint(ref);
  }

  // Add the IR step
  build.addStep(shared_from_this(), make_shared<ContentsMatch>(ref, v));

  // Report the read
  _content_filter.read(this, ref);
}

// This command depends on the destination of a symlink
void Command::symlinkMatch(Build& build, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a content match on an unresolved reference: "
                            << ref;

  // Inform the artifact that this command accesses its contents
  const auto& v = ref->getArtifact()->readlink(shared_from_this(), InputType::Accessed);

  if (!v) {
    WARN << "Readlink on " << ref << " returned a null version";
    return;
  }

  // Take a fingerprint for consistency, although symlinks are always fully saved
  v->fingerprint(ref);

  // Add the IR step
  build.addStep(shared_from_this(), make_shared<SymlinkMatch>(ref, v));
}

// This command sets the metadata of a referenced artifact
void Command::setMetadata(Build& build, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot set metadata for an unresolved reference.";

  // Do we have to log this write?
  if (!_metadata_filter.writeRequired(this, ref)) return;

  // Inform the artifact that this command sets its metadata
  const auto& v = ref->getArtifact()->setMetadata(shared_from_this(), ref);

  // Create the SetMetadata step and add it to the command
  build.addStep(shared_from_this(), make_shared<SetMetadata>(ref, v));

  // Report the write
  _metadata_filter.write(this, ref, v);
}

// This command sets the contents of a referenced artifact
void Command::setContents(Build& build, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot set contents for an unresolved reference.";

  // Do we have to log this write?
  if (!_content_filter.writeRequired(this, ref)) return;

  // Inform the artifact that this command sets its contents
  const auto& v = ref->getArtifact()->setContents(shared_from_this(), ref);

  ASSERT(v) << "Setting contents of " << ref << " produced a null version";

  // Create the SetContents step and add it to the command
  build.addStep(shared_from_this(), make_shared<SetContents>(ref, v));

  // Report the write
  _content_filter.write(this, ref, v);
}

// This command adds an entry to a directory
void Command::link(Build& build,
                   shared_ptr<Reference> ref,
                   string entry,
                   shared_ptr<Reference> target) noexcept {
  ASSERT(ref->isResolved()) << "Cannot remove an entry from an unresolved directory";
  ASSERT(target->isResolved()) << "Cannot link an unresolved reference into a directory";

  ref->getArtifact()->addEntry(shared_from_this(), ref, entry, target);

  build.addStep(shared_from_this(), make_shared<Link>(ref, entry, target));
}

// This command removes an entry from a directory
void Command::unlink(Build& build, shared_ptr<Reference> ref, string entry) noexcept {
  ASSERT(ref->isResolved()) << "Cannot remove an entry from an unresolved directory";

  ref->getArtifact()->removeEntry(shared_from_this(), ref, entry);

  build.addStep(shared_from_this(), make_shared<Unlink>(ref, entry));
}

// This command launches a child command
const shared_ptr<Command>& Command::launch(Build& build,
                                           shared_ptr<Access> exe,
                                           vector<string> args,
                                           map<int, FileDescriptor> fds,
                                           shared_ptr<Access> cwd,
                                           shared_ptr<Access> root) noexcept {
  auto child = make_shared<Command>(exe, args, fds, cwd, root);

  if (options::print_on_run) cout << child->getShortName(options::command_length) << endl;

  // The child command depends on all current versions of the artifacts in its fd table
  for (auto& [index, desc] : fds) {
    desc.getReference()->getArtifact()->needsCurrentVersions(child);
  }

  child->setExecuted();

  build.addStep(shared_from_this(), make_shared<Launch>(child));
  return _children.emplace_back(child);
}

// This command joined with a child command
void Command::join(Build& build, shared_ptr<Command> child, int exit_status) noexcept {
  LOG << this << " joined command " << child << " with exit status " << exit_status;

  // Save the exit status in the child
  child->_exit_status = exit_status;

  // Add a join action to this command's steps
  build.addStep(shared_from_this(), make_shared<Join>(child, exit_status));
}
