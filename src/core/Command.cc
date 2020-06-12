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

// The root command invokes "dodo launch" to run the actual build script. Construct this command.
shared_ptr<Command> Command::createRootCommand() noexcept {
  // We need to get the path to dodo. Use readlink for this.
  fs::path dodo = readlink("/proc/self/exe");
  fs::path dodo_launch = dodo.parent_path() / "dodo-launch";

  auto stdin_ref = make_shared<Pipe>();
  auto stdout_ref = make_shared<Pipe>();
  auto stderr_ref = make_shared<Pipe>();

  map<int, FileDescriptor> default_fds = {{0, FileDescriptor(stdin_ref, false)},
                                          {1, FileDescriptor(stdout_ref, true)},
                                          {2, FileDescriptor(stderr_ref, true)}};

  auto cwd = Access::createCwd(AccessFlags{.x = true});
  auto root = Access::createRoot(AccessFlags{.x = true});
  auto exe = make_shared<Access>(root, dodo_launch, AccessFlags{.r = true});

  return shared_ptr<Command>(new Command(exe, {"dodo-launch"}, default_fds, cwd, root));
}

string Command::getShortName() const noexcept {
  // By default, the short name is the executable
  string result = _exe->getPath();

  // If we have arguments, use args[0] instead of the exe name
  if (_args.size() > 0) result = _args.front();

  // Strip path from the base name
  auto pos = result.rfind('/');
  if (pos != string::npos) {
    result = result.substr(pos + 1);
  }

  // Add arguments up to a length of 20 characters
  size_t index = 1;
  while (index < _args.size() && result.length() < 20) {
    result += " " + _args[index];
    index++;
  }

  if (result.length() >= 20) {
    result = result.substr(0, 17) + "...";
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

void Command::emulate(Build& build) noexcept {
  // If this command has never run, report it as changed
  if (_steps.empty()) build.observeCommandNeverRun(shared_from_this());

  // Resolve the reference to this command's executable
  _exe->resolve(shared_from_this(), build);

  for (const auto& step : _steps) {
    step->emulate(shared_from_this(), build);
  }
}

// This command accesses an artifact by path.
shared_ptr<Access> Command::access(fs::path path, AccessFlags flags,
                                   shared_ptr<Access> base) noexcept {
  // In path resolution, it may be helpful to store each level of the path in a new reference.
  // Uncomment to re-enable this.
  /*auto current = base;
  for (const auto& entry : path) {
    if (entry != "." && entry != "/") {
      current = current->get(entry, AccessFlags{.x = true});
    }
  }

  auto ref = current->withFlags(flags);*/

  // For now, the reference is just one level that covers all parts of the new path
  auto ref = base->get(path, flags);
  _steps.emplace_back(ref);
  return ref;
}

// Make an access using a new set of flags
shared_ptr<Access> Command::access(shared_ptr<Access> a, AccessFlags flags) noexcept {
  auto ref = a->withFlags(flags);
  _steps.emplace_back(ref);
  return ref;
}

// This command creates a reference to a new pipe
shared_ptr<Pipe> Command::pipe() noexcept {
  auto ref = make_shared<Pipe>();
  _steps.emplace_back(ref);
  return ref;
}

// This command depends on the metadata of a referenced artifact
void Command::metadataMatch(shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a metadata match on an unresolved reference.";

  // Do we have to log this read?
  if (!_metadata_filter.readRequired(this, ref)) return;

  // Inform the artifact that this command accesses its metadata
  const auto& v = ref->getArtifact()->accessMetadata(shared_from_this(), ref);

  // If the last write was from this command we don't need a fingerprint to compare.
  if (v->getCreator() != shared_from_this()) {
    v->fingerprint(ref);
  }

  // Add the IR step
  _steps.push_back(make_shared<MetadataMatch>(ref, v));

  // Report the read
  _metadata_filter.read(this, ref);
}

// This command depends on the contents of a referenced artifact
void Command::contentsMatch(shared_ptr<Reference> ref) noexcept {
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
  _steps.push_back(make_shared<ContentsMatch>(ref, v));

  // Report the read
  _content_filter.read(this, ref);
}

// This command sets the metadata of a referenced artifact
void Command::setMetadata(shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot set metadata for an unresolved reference.";

  // Do we have to log this write?
  if (!_metadata_filter.writeRequired(this, ref)) return;

  // Inform the artifact that this command sets its metadata
  const auto& v = ref->getArtifact()->setMetadata(shared_from_this(), ref);

  // Create the SetMetadata step and add it to the command
  _steps.push_back(make_shared<SetMetadata>(ref, v));

  // Report the write
  _metadata_filter.write(this, ref, v);
}

// This command sets the contents of a referenced artifact
void Command::setContents(shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot set contents for an unresolved reference.";

  // Do we have to log this write?
  if (!_content_filter.writeRequired(this, ref)) return;

  // Inform the artifact that this command sets its contents
  const auto& v = ref->getArtifact()->setContents(shared_from_this(), ref);

  ASSERT(v) << "Setting contents of " << ref << " produced a null version";

  // Create the SetContents step and add it to the command
  _steps.push_back(make_shared<SetContents>(ref, v));

  // Report the write
  _content_filter.write(this, ref, v);
}

// This command launches a child command
const shared_ptr<Command>& Command::launch(shared_ptr<Access> exe, vector<string> args,
                                           map<int, FileDescriptor> fds, shared_ptr<Access> cwd,
                                           shared_ptr<Access> root) noexcept {
  auto child = make_shared<Command>(exe, args, fds, cwd, root);

  if (options::print_on_run) cout << child->getFullName() << endl;

  _steps.emplace_back(make_shared<Launch>(child));
  return _children.emplace_back(child);
}
