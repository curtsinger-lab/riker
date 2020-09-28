#include "Trace.hh"

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <fcntl.h>
#include <sys/stat.h>

#include <cereal/archives/binary.hpp>
#include <cereal/types/array.hpp>
#include <cereal/types/list.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/optional.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/set.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/vector.hpp>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "build/Build.hh"
#include "build/Command.hh"
#include "build/Env.hh"
#include "build/RefResult.hh"
#include "core/AccessFlags.hh"
#include "core/FileDescriptor.hh"
#include "util/log.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::endl;
using std::make_shared;
using std::make_unique;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

enum : size_t { ArchiveMagic = 0xD0D0D035178357, ArchiveVersion = 101 };

struct CommandRecord : public Record {
  Command::ID _id;
  RefResult::ID _root_id;
  RefResult::ID _cwd_id;
  RefResult::ID _exe_id;
  vector<string> _args;
  map<int, tuple<RefResult::ID, AccessFlags>> _initial_fds;
  bool _executed;
  int _exit_status;

  /// Default constructor for serialization
  CommandRecord() noexcept = default;

  CommandRecord(Command::ID id,
                RefResult::ID root_id,
                RefResult::ID cwd_id,
                RefResult::ID exe_id,
                vector<string> args,
                map<int, tuple<RefResult::ID, AccessFlags>> initial_fds,
                bool executed,
                int exit_status) :
      _id(id),
      _root_id(root_id),
      _cwd_id(cwd_id),
      _exe_id(exe_id),
      _args(args),
      _initial_fds(initial_fds),
      _executed(executed),
      _exit_status(exit_status) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    map<int, FileDescriptor> fds;
    for (auto [fd, info] : _initial_fds) {
      auto [ref_id, flags] = info;
      fds[fd] = FileDescriptor(input.getRefResult(ref_id), flags);
    }

    auto cmd = make_shared<Command>(input.getRefResult(_exe_id), _args, fds,
                                    input.getRefResult(_cwd_id), input.getRefResult(_root_id));
    if (_executed) cmd->setExecuted();
    cmd->setExitStatus(_exit_status);

    input.addCommand(_id, cmd);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _id, _root_id, _cwd_id, _exe_id, _args, _initial_fds,
            _executed, _exit_status);
  }
};

CEREAL_REGISTER_TYPE(CommandRecord);

struct SpecialRefRecord : public Record {
  Command::ID _cmd;
  SpecialRef _entity;
  RefResult::ID _output;

  /// Default constructor for serialization
  SpecialRefRecord() noexcept = default;

  SpecialRefRecord(Command::ID cmd, SpecialRef entity, RefResult::ID output) noexcept :
      _cmd(cmd), _entity(entity), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.specialRef(input.getCommand(_cmd), _entity, input.getRefResult(_output));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _entity, _output);
  }
};

CEREAL_REGISTER_TYPE(SpecialRefRecord);

struct PipeRefRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _read_end;
  RefResult::ID _write_end;

  /// Default constructor for serialization
  PipeRefRecord() noexcept = default;

  PipeRefRecord(Command::ID cmd, RefResult::ID read_end, RefResult::ID write_end) noexcept :
      _cmd(cmd), _read_end(read_end), _write_end(write_end) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.pipeRef(input.getCommand(_cmd), input.getRefResult(_read_end),
                    input.getRefResult(_write_end));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _read_end, _write_end);
  }
};

CEREAL_REGISTER_TYPE(PipeRefRecord);

struct FileRefRecord : public Record {
  Command::ID _cmd;
  mode_t _mode;
  RefResult::ID _output;

  /// Default constructor for serialization
  FileRefRecord() noexcept = default;

  FileRefRecord(Command::ID cmd, mode_t mode, RefResult::ID output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.fileRef(input.getCommand(_cmd), _mode, input.getRefResult(_output));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

CEREAL_REGISTER_TYPE(FileRefRecord);

struct SymlinkRefRecord : public Record {
  Command::ID _cmd;
  fs::path _target;
  RefResult::ID _output;

  /// Default constructor for serialization
  SymlinkRefRecord() noexcept = default;

  SymlinkRefRecord(Command::ID cmd, fs::path target, RefResult::ID output) noexcept :
      _cmd(cmd), _target(target), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.symlinkRef(input.getCommand(_cmd), _target, input.getRefResult(_output));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _target, _output);
  }
};

CEREAL_REGISTER_TYPE(SymlinkRefRecord);

struct DirRefRecord : public Record {
  Command::ID _cmd;
  mode_t _mode;
  RefResult::ID _output;

  /// Default constructor for serialization
  DirRefRecord() noexcept = default;

  DirRefRecord(Command::ID cmd, mode_t mode, RefResult::ID output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.dirRef(input.getCommand(_cmd), _mode, input.getRefResult(_output));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

CEREAL_REGISTER_TYPE(DirRefRecord);

struct PathRefRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _base;
  fs::path _path;
  AccessFlags _flags;
  RefResult::ID _output;

  /// Default constructor for serialization
  PathRefRecord() noexcept = default;

  PathRefRecord(Command::ID cmd,
                RefResult::ID base,
                fs::path path,
                AccessFlags flags,
                RefResult::ID output) noexcept :
      _cmd(cmd), _base(base), _path(path), _flags(flags), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.pathRef(input.getCommand(_cmd), input.getRefResult(_base), _path, _flags,
                    input.getRefResult(_output));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _base, _path, _flags, _output);
  }
};

CEREAL_REGISTER_TYPE(PathRefRecord);

struct ExpectResultRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _ref;
  int _expected;

  /// Default constructor for serialization
  ExpectResultRecord() noexcept = default;

  ExpectResultRecord(Command::ID cmd, RefResult::ID ref, int expected) noexcept :
      _cmd(cmd), _ref(ref), _expected(expected) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.expectResult(input.getCommand(_cmd), input.getRefResult(_ref), _expected);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _expected);
  }
};

CEREAL_REGISTER_TYPE(ExpectResultRecord);

struct MatchMetadataRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _ref;
  shared_ptr<MetadataVersion> _version;

  /// Default constructor for serialization
  MatchMetadataRecord() noexcept = default;

  MatchMetadataRecord(Command::ID cmd,
                      RefResult::ID ref,
                      shared_ptr<MetadataVersion> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.matchMetadata(input.getCommand(_cmd), input.getRefResult(_ref), _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(MatchMetadataRecord);

struct MatchContentRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _ref;
  shared_ptr<Version> _version;

  /// Default constructor for serialization
  MatchContentRecord() noexcept = default;

  MatchContentRecord(Command::ID cmd, RefResult::ID ref, shared_ptr<Version> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.matchContent(input.getCommand(_cmd), input.getRefResult(_ref), _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(MatchContentRecord);

struct UpdateMetadataRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _ref;
  shared_ptr<MetadataVersion> _version;

  /// Default constructor for serialization
  UpdateMetadataRecord() noexcept = default;

  UpdateMetadataRecord(Command::ID cmd,
                       RefResult::ID ref,
                       shared_ptr<MetadataVersion> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.updateMetadata(input.getCommand(_cmd), input.getRefResult(_ref), _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(UpdateMetadataRecord);

struct UpdateContentRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _ref;
  shared_ptr<Version> _version;

  /// Default constructor for serialization
  UpdateContentRecord() noexcept = default;

  UpdateContentRecord(Command::ID cmd, RefResult::ID ref, shared_ptr<Version> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.updateContent(input.getCommand(_cmd), input.getRefResult(_ref), _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(UpdateContentRecord);

struct AddEntryRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _dir;
  fs::path _name;
  RefResult::ID _target;

  /// Default constructor for serialization
  AddEntryRecord() noexcept = default;

  AddEntryRecord(Command::ID cmd, RefResult::ID dir, fs::path name, RefResult::ID target) noexcept :
      _cmd(cmd), _dir(dir), _name(name), _target(target) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.addEntry(input.getCommand(_cmd), input.getRefResult(_dir), _name,
                     input.getRefResult(_target));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _dir, _name, _target);
  }
};

CEREAL_REGISTER_TYPE(AddEntryRecord);

struct RemoveEntryRecord : public Record {
  Command::ID _cmd;
  RefResult::ID _dir;
  fs::path _name;
  RefResult::ID _target;

  /// Default constructor for serialization
  RemoveEntryRecord() noexcept = default;

  RemoveEntryRecord(Command::ID cmd,
                    RefResult::ID dir,
                    fs::path name,
                    RefResult::ID target) noexcept :
      _cmd(cmd), _dir(dir), _name(name), _target(target) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.removeEntry(input.getCommand(_cmd), input.getRefResult(_dir), _name,
                        input.getRefResult(_target));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _dir, _name, _target);
  }
};

CEREAL_REGISTER_TYPE(RemoveEntryRecord);

struct LaunchRecord : public Record {
  Command::ID _cmd;
  Command::ID _child;

  /// Default constructor for serialization
  LaunchRecord() noexcept = default;

  LaunchRecord(Command::ID cmd, Command::ID child) noexcept : _cmd(cmd), _child(child) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.launch(input.getCommand(_cmd), input.getCommand(_child));
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _child);
  }
};

CEREAL_REGISTER_TYPE(LaunchRecord);

struct JoinRecord : public Record {
  Command::ID _cmd;
  Command::ID _child;
  int _exit_status;

  /// Default constructor for serialization
  JoinRecord() noexcept = default;

  JoinRecord(Command::ID cmd, Command::ID child, int exit_status) noexcept :
      _cmd(cmd), _child(child), _exit_status(exit_status) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.join(input.getCommand(_cmd), input.getCommand(_child), _exit_status);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _child, _exit_status);
  }
};

CEREAL_REGISTER_TYPE(JoinRecord);

struct ExitRecord : public Record {
  Command::ID _cmd;
  int _exit_status;

  /// Default constructor for serialization
  ExitRecord() noexcept = default;

  ExitRecord(Command::ID cmd, int exit_status) noexcept : _cmd(cmd), _exit_status(exit_status) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.exit(input.getCommand(_cmd), _exit_status);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _exit_status);
  }
};

CEREAL_REGISTER_TYPE(ExitRecord);

struct EndRecord : public Record {
  EndRecord() noexcept = default;

  virtual bool isEnd() const noexcept override { return true; }

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {}

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this));
  }
};

CEREAL_REGISTER_TYPE(EndRecord);

/*********************************/

// Run this trace
void InputTrace::sendTo(TraceHandler& handler) noexcept {
  bool use_default = false;

  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(_filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Load the version header from the trace file
    size_t magic;
    size_t version;
    archive(magic, version);

    // Check the magic number and version
    if (magic != ArchiveMagic) {
      WARN << "Saved trace does appears to be invalid. Running a full build.";
      use_default = true;
    } else if (version != ArchiveVersion) {
      WARN << "Saved trace is not the correct version. Running a full build.";
      use_default = true;
    }

    // If we are not using a default trace, load it now
    if (!use_default) {
      // Loop until we hit the end of the trace
      bool done = false;
      while (!done) {
        unique_ptr<Record> record;
        archive(record);
        done = record->isEnd();
        record->handle(*this, handler);
      }
    }

  } catch (cereal::Exception& e) {
    // If there is an exception when loading the trace, revert to a default trace
    use_default = true;
  }

  if (use_default) sendDefault(handler);

  handler.finish();
}

void InputTrace::sendDefault(TraceHandler& handler) noexcept {
  Command::ID no_cmd_id = 0;
  auto no_cmd = getCommand(no_cmd_id);

  // Create the initial pipe references
  RefResult::ID stdin_ref_id = 0;
  auto stdin_ref = getRefResult(stdin_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stdin, stdin_ref);

  RefResult::ID stdout_ref_id = 1;
  auto stdout_ref = getRefResult(stdout_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stdout, stdout_ref);

  RefResult::ID stderr_ref_id = 2;
  auto stderr_ref = getRefResult(stderr_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stderr, stderr_ref);

  // Create a reference to the root directory
  RefResult::ID root_ref_id = 3;
  auto root_ref = getRefResult(root_ref_id);
  handler.specialRef(no_cmd, SpecialRef::root, root_ref);

  // Create a reference to the current working directory and add it to the trace
  RefResult::ID cwd_ref_id = 4;
  auto cwd_ref = getRefResult(cwd_ref_id);
  handler.specialRef(no_cmd, SpecialRef::cwd, cwd_ref);

  // Set up the reference to the dodo-launch executable and add it to the trace
  RefResult::ID exe_ref_id = 5;
  auto exe_ref = getRefResult(exe_ref_id);
  handler.specialRef(no_cmd, SpecialRef::launch_exe, exe_ref);

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin_ref, AccessFlags{.r = true})},
                                  {1, FileDescriptor(stdout_ref, AccessFlags{.w = true})},
                                  {2, FileDescriptor(stderr_ref, AccessFlags{.w = true})}};

  // Create a root command
  Command::ID root_cmd_id = 1;
  addCommand(root_cmd_id,
             make_shared<Command>(exe_ref, vector<string>{"dodo-launch"}, fds, cwd_ref, root_ref));

  // Launch the root command
  handler.launch(no_cmd, getCommand(root_cmd_id));
}

/// Add a SpecialRef IR step to the output trace
void OutputTrace::specialRef(shared_ptr<Command> cmd,
                             SpecialRef entity,
                             shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new SpecialRefRecord(getCommandID(cmd), entity, getRefResultID(output)));
}

/// Add a PipeRef IR step to the output trace
void OutputTrace::pipeRef(shared_ptr<Command> cmd,
                          shared_ptr<RefResult> read_end,
                          shared_ptr<RefResult> write_end) noexcept {
  _records.emplace_back(
      new PipeRefRecord(getCommandID(cmd), getRefResultID(read_end), getRefResultID(write_end)));
}

/// Add a FileRef IR step to the output trace
void OutputTrace::fileRef(shared_ptr<Command> cmd,
                          mode_t mode,
                          shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new FileRefRecord(getCommandID(cmd), mode, getRefResultID(output)));
}

/// Add a SymlinkRef IR step to the output trace
void OutputTrace::symlinkRef(shared_ptr<Command> cmd,
                             fs::path target,
                             shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new SymlinkRefRecord(getCommandID(cmd), target, getRefResultID(output)));
}

/// Add a DirRef IR step to the output trace
void OutputTrace::dirRef(shared_ptr<Command> cmd,
                         mode_t mode,
                         shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new DirRefRecord(getCommandID(cmd), mode, getRefResultID(output)));
}

/// Add a PathRef IR step to the output trace
void OutputTrace::pathRef(shared_ptr<Command> cmd,
                          shared_ptr<RefResult> base,
                          fs::path path,
                          AccessFlags flags,
                          shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new PathRefRecord(getCommandID(cmd), getRefResultID(base), path, flags,
                                          getRefResultID(output)));
}

/// Add a ExpectResult IR step to the output trace
void OutputTrace::expectResult(shared_ptr<Command> cmd,
                               shared_ptr<RefResult> ref,
                               int expected) noexcept {
  _records.emplace_back(new ExpectResultRecord(getCommandID(cmd), getRefResultID(ref), expected));
}

/// Add a MatchMetadata IR step to the output trace
void OutputTrace::matchMetadata(shared_ptr<Command> cmd,
                                shared_ptr<RefResult> ref,
                                shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new MatchMetadataRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add a MatchContent IR step to the output trace
void OutputTrace::matchContent(shared_ptr<Command> cmd,
                               shared_ptr<RefResult> ref,
                               shared_ptr<Version> version) noexcept {
  _records.emplace_back(new MatchContentRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add a UpdateMetadata IR step to the output trace
void OutputTrace::updateMetadata(shared_ptr<Command> cmd,
                                 shared_ptr<RefResult> ref,
                                 shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new UpdateMetadataRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add a UpdateContent IR step to the output trace
void OutputTrace::updateContent(shared_ptr<Command> cmd,
                                shared_ptr<RefResult> ref,
                                shared_ptr<Version> version) noexcept {
  _records.emplace_back(new UpdateContentRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add an AddEntry IR step to the output trace
void OutputTrace::addEntry(shared_ptr<Command> cmd,
                           shared_ptr<RefResult> dir,
                           fs::path name,
                           shared_ptr<RefResult> target) noexcept {
  _records.emplace_back(
      new AddEntryRecord(getCommandID(cmd), getRefResultID(dir), name, getRefResultID(target)));
}

/// Add a RemoveEntry IR step to the output trace
void OutputTrace::removeEntry(shared_ptr<Command> cmd,
                              shared_ptr<RefResult> dir,
                              fs::path name,
                              shared_ptr<RefResult> target) noexcept {
  _records.emplace_back(
      new RemoveEntryRecord(getCommandID(cmd), getRefResultID(dir), name, getRefResultID(target)));
}

/// Add a Launch IR step to the output trace
void OutputTrace::launch(shared_ptr<Command> cmd, shared_ptr<Command> child) noexcept {
  // Add the launched command to the set of commands
  Command::ID child_id = addCommand(child);
  RefResult::ID root_id = getRefResultID(child->getInitialRootDir());
  RefResult::ID cwd_id = getRefResultID(child->getInitialWorkingDir());
  RefResult::ID exe_id = getRefResultID(child->getExecutable());

  map<int, tuple<RefResult::ID, AccessFlags>> fds;
  for (auto [fd, info] : child->getInitialFDs()) {
    fds[fd] = {getRefResultID(info.getRef()), info.getFlags()};
  }

  _records.emplace_back(new CommandRecord(child_id, root_id, cwd_id, exe_id, child->getArguments(),
                                          fds, child->hasExecuted(), child->getExitStatus()));

  // Create the record for the launch IR step
  _records.emplace_back(new LaunchRecord(getCommandID(cmd), getCommandID(child)));
}

/// Add a Join IR step to the output trace
void OutputTrace::join(shared_ptr<Command> cmd,
                       shared_ptr<Command> child,
                       int exit_status) noexcept {
  _records.emplace_back(new JoinRecord(getCommandID(cmd), getCommandID(child), exit_status));
}

/// Add a Exit IR step to the output trace
void OutputTrace::exit(shared_ptr<Command> cmd, int exit_status) noexcept {
  _records.emplace_back(new ExitRecord(getCommandID(cmd), exit_status));
}

void OutputTrace::finish() noexcept {
  ofstream out(_filename, std::ios::binary);
  cereal::BinaryOutputArchive archive(out);

  // Write out the magic number and version
  archive(ArchiveMagic, ArchiveVersion);

  // Write out the list of records
  for (auto& r : _records) {
    archive(r);
  }

  unique_ptr<Record> end(new EndRecord());
  // Mark the end of the trace
  archive(end);
}

/// Serialization function for struct timespec
template <class Archive>
void serialize(Archive& ar, struct timespec& ts) noexcept {
  ar(ts.tv_sec, ts.tv_nsec);
}

/// Serialization function for std::filesystem::path
namespace std {
  namespace filesystem {
    template <class Archive>
    void load(Archive& ar, path& p) {
      string s;
      ar(s);
      p = s;
    }

    template <class Archive>
    void save(Archive& ar, const path& p) {
      ar(string(p));
    }
  }
}

/** Register types and polymorphic relationships **/

// Versions
CEREAL_REGISTER_TYPE(MetadataVersion);
CEREAL_REGISTER_TYPE(FileVersion);
CEREAL_REGISTER_TYPE(SymlinkVersion);
CEREAL_REGISTER_TYPE(DirListVersion);
