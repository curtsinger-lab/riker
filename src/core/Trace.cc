#include "Trace.hh"

#include <iostream>
#include <map>
#include <memory>
#include <string>
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
#include "build/Env.hh"
#include "core/AccessFlags.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/RefResult.hh"
#include "core/SpecialRefs.hh"
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
using std::vector;

struct CommandRecord : public Record {
  command_id_t _id;
  shared_ptr<Command> _cmd;

  /// Default constructor for serialization
  CommandRecord() noexcept = default;

  CommandRecord(command_id_t id, shared_ptr<Command> cmd) : _id(id), _cmd(cmd) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    input.addCommand(_id, _cmd);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _id, _cmd);
  }
};

CEREAL_REGISTER_TYPE(CommandRecord);

struct SpecialRefRecord : public Record {
  command_id_t _cmd;
  SpecialRef _entity;
  shared_ptr<RefResult> _output;

  /// Default constructor for serialization
  SpecialRefRecord() noexcept = default;

  SpecialRefRecord(command_id_t cmd, SpecialRef entity, shared_ptr<RefResult> output) noexcept :
      _cmd(cmd), _entity(entity), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.specialRef(input.getCommand(_cmd), _entity, _output);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _entity, _output);
  }
};

CEREAL_REGISTER_TYPE(SpecialRefRecord);

struct PipeRefRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _read_end;
  shared_ptr<RefResult> _write_end;

  /// Default constructor for serialization
  PipeRefRecord() noexcept = default;

  PipeRefRecord(command_id_t cmd,
                shared_ptr<RefResult> read_end,
                shared_ptr<RefResult> write_end) noexcept :
      _cmd(cmd), _read_end(read_end), _write_end(write_end) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.pipeRef(input.getCommand(_cmd), _read_end, _write_end);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _read_end, _write_end);
  }
};

CEREAL_REGISTER_TYPE(PipeRefRecord);

struct FileRefRecord : public Record {
  command_id_t _cmd;
  mode_t _mode;
  shared_ptr<RefResult> _output;

  /// Default constructor for serialization
  FileRefRecord() noexcept = default;

  FileRefRecord(command_id_t cmd, mode_t mode, shared_ptr<RefResult> output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.fileRef(input.getCommand(_cmd), _mode, _output);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

CEREAL_REGISTER_TYPE(FileRefRecord);

struct SymlinkRefRecord : public Record {
  command_id_t _cmd;
  fs::path _target;
  shared_ptr<RefResult> _output;

  /// Default constructor for serialization
  SymlinkRefRecord() noexcept = default;

  SymlinkRefRecord(command_id_t cmd, fs::path target, shared_ptr<RefResult> output) noexcept :
      _cmd(cmd), _target(target), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.symlinkRef(input.getCommand(_cmd), _target, _output);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _target, _output);
  }
};

CEREAL_REGISTER_TYPE(SymlinkRefRecord);

struct DirRefRecord : public Record {
  command_id_t _cmd;
  mode_t _mode;
  shared_ptr<RefResult> _output;

  /// Default constructor for serialization
  DirRefRecord() noexcept = default;

  DirRefRecord(command_id_t cmd, mode_t mode, shared_ptr<RefResult> output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.dirRef(input.getCommand(_cmd), _mode, _output);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

CEREAL_REGISTER_TYPE(DirRefRecord);

struct PathRefRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _base;
  fs::path _path;
  AccessFlags _flags;
  shared_ptr<RefResult> _output;

  /// Default constructor for serialization
  PathRefRecord() noexcept = default;

  PathRefRecord(command_id_t cmd,
                shared_ptr<RefResult> base,
                fs::path path,
                AccessFlags flags,
                shared_ptr<RefResult> output) noexcept :
      _cmd(cmd), _base(base), _path(path), _flags(flags), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.pathRef(input.getCommand(_cmd), _base, _path, _flags, _output);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _base, _path, _flags, _output);
  }
};

CEREAL_REGISTER_TYPE(PathRefRecord);

struct ExpectResultRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _ref;
  int _expected;

  /// Default constructor for serialization
  ExpectResultRecord() noexcept = default;

  ExpectResultRecord(command_id_t cmd, shared_ptr<RefResult> ref, int expected) noexcept :
      _cmd(cmd), _ref(ref), _expected(expected) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.expectResult(input.getCommand(_cmd), _ref, _expected);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _expected);
  }
};

CEREAL_REGISTER_TYPE(ExpectResultRecord);

struct MatchMetadataRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _ref;
  shared_ptr<MetadataVersion> _version;

  /// Default constructor for serialization
  MatchMetadataRecord() noexcept = default;

  MatchMetadataRecord(command_id_t cmd,
                      shared_ptr<RefResult> ref,
                      shared_ptr<MetadataVersion> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.matchMetadata(input.getCommand(_cmd), _ref, _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(MatchMetadataRecord);

struct MatchContentRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _ref;
  shared_ptr<Version> _version;

  /// Default constructor for serialization
  MatchContentRecord() noexcept = default;

  MatchContentRecord(command_id_t cmd,
                     shared_ptr<RefResult> ref,
                     shared_ptr<Version> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.matchContent(input.getCommand(_cmd), _ref, _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(MatchContentRecord);

struct UpdateMetadataRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _ref;
  shared_ptr<MetadataVersion> _version;

  /// Default constructor for serialization
  UpdateMetadataRecord() noexcept = default;

  UpdateMetadataRecord(command_id_t cmd,
                       shared_ptr<RefResult> ref,
                       shared_ptr<MetadataVersion> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.updateMetadata(input.getCommand(_cmd), _ref, _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(UpdateMetadataRecord);

struct UpdateContentRecord : public Record {
  command_id_t _cmd;
  shared_ptr<RefResult> _ref;
  shared_ptr<Version> _version;

  /// Default constructor for serialization
  UpdateContentRecord() noexcept = default;

  UpdateContentRecord(command_id_t cmd,
                      shared_ptr<RefResult> ref,
                      shared_ptr<Version> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override {
    handler.updateContent(input.getCommand(_cmd), _ref, _version);
  }

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

CEREAL_REGISTER_TYPE(UpdateContentRecord);

struct LaunchRecord : public Record {
  command_id_t _cmd;
  command_id_t _child;

  /// Default constructor for serialization
  LaunchRecord() noexcept = default;

  LaunchRecord(command_id_t cmd, command_id_t child) noexcept : _cmd(cmd), _child(child) {}

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
  command_id_t _cmd;
  command_id_t _child;
  int _exit_status;

  /// Default constructor for serialization
  JoinRecord() noexcept = default;

  JoinRecord(command_id_t cmd, command_id_t child, int exit_status) noexcept :
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
  command_id_t _cmd;
  int _exit_status;

  /// Default constructor for serialization
  ExitRecord() noexcept = default;

  ExitRecord(command_id_t cmd, int exit_status) noexcept : _cmd(cmd), _exit_status(exit_status) {}

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

InputTrace::InputTrace(string filename) noexcept {
  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Loop until we hit the end of the trace
    bool done = false;
    while (!done) {
      unique_ptr<Record> record;
      archive(record);
      done = record->isEnd();
      _records.emplace_back(std::move(record));
    }
  } catch (cereal::Exception& e) {
    initDefault();
  }
}

void InputTrace::initDefault() noexcept {
  // Clear the list of records
  _records.clear();

  // Create the initial pipe references
  auto stdin_ref = make_shared<RefResult>();
  _records.emplace_back(new SpecialRefRecord(0, SpecialRef::stdin, stdin_ref));

  auto stdout_ref = make_shared<RefResult>();
  _records.emplace_back(new SpecialRefRecord(0, SpecialRef::stdout, stdout_ref));

  auto stderr_ref = make_shared<RefResult>();
  _records.emplace_back(new SpecialRefRecord(0, SpecialRef::stderr, stderr_ref));

  // Create a reference to the root directory
  auto root_ref = make_shared<RefResult>();
  _records.emplace_back(new SpecialRefRecord(0, SpecialRef::root, root_ref));

  // Create a reference to the current working directory and add it to the trace
  auto cwd_ref = make_shared<RefResult>();
  _records.emplace_back(new SpecialRefRecord(0, SpecialRef::cwd, cwd_ref));

  // Set up the reference to the dodo-launch executable and add it to the trace
  auto exe_ref = make_shared<RefResult>();
  _records.emplace_back(new SpecialRefRecord(0, SpecialRef::launch_exe, exe_ref));

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin_ref, AccessFlags{.r = true})},
                                  {1, FileDescriptor(stdout_ref, AccessFlags{.w = true})},
                                  {2, FileDescriptor(stderr_ref, AccessFlags{.w = true})}};

  // Make a root cmd
  auto root_cmd =
      make_shared<Command>(exe_ref, vector<string>{"dodo-launch"}, fds, cwd_ref, root_ref);

  // Create a record of the root command with ID 2
  _records.emplace_back(new CommandRecord(1, root_cmd));

  // Make a launch action for the root cmd
  _records.emplace_back(new LaunchRecord(0, 1));
}

// Run this trace
void InputTrace::sendTo(TraceHandler& handler) noexcept {
  for (auto& record : _records) {
    record->handle(*this, handler);
  }
  handler.finish();
}

/// Add a SpecialRef IR step to the output trace
void OutputTrace::specialRef(shared_ptr<Command> cmd,
                             SpecialRef entity,
                             shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new SpecialRefRecord(getCommandID(cmd), entity, output));
}

/// Add a PipeRef IR step to the output trace
void OutputTrace::pipeRef(shared_ptr<Command> cmd,
                          shared_ptr<RefResult> read_end,
                          shared_ptr<RefResult> write_end) noexcept {
  _records.emplace_back(new PipeRefRecord(getCommandID(cmd), read_end, write_end));
}

/// Add a FileRef IR step to the output trace
void OutputTrace::fileRef(shared_ptr<Command> cmd,
                          mode_t mode,
                          shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new FileRefRecord(getCommandID(cmd), mode, output));
}

/// Add a SymlinkRef IR step to the output trace
void OutputTrace::symlinkRef(shared_ptr<Command> cmd,
                             fs::path target,
                             shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new SymlinkRefRecord(getCommandID(cmd), target, output));
}

/// Add a DirRef IR step to the output trace
void OutputTrace::dirRef(shared_ptr<Command> cmd,
                         mode_t mode,
                         shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new DirRefRecord(getCommandID(cmd), mode, output));
}

/// Add a PathRef IR step to the output trace
void OutputTrace::pathRef(shared_ptr<Command> cmd,
                          shared_ptr<RefResult> base,
                          fs::path path,
                          AccessFlags flags,
                          shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new PathRefRecord(getCommandID(cmd), base, path, flags, output));
}

/// Add a ExpectResult IR step to the output trace
void OutputTrace::expectResult(shared_ptr<Command> cmd,
                               shared_ptr<RefResult> ref,
                               int expected) noexcept {
  _records.emplace_back(new ExpectResultRecord(getCommandID(cmd), ref, expected));
}

/// Add a MatchMetadata IR step to the output trace
void OutputTrace::matchMetadata(shared_ptr<Command> cmd,
                                shared_ptr<RefResult> ref,
                                shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new MatchMetadataRecord(getCommandID(cmd), ref, version));
}

/// Add a MatchContent IR step to the output trace
void OutputTrace::matchContent(shared_ptr<Command> cmd,
                               shared_ptr<RefResult> ref,
                               shared_ptr<Version> version) noexcept {
  _records.emplace_back(new MatchContentRecord(getCommandID(cmd), ref, version));
}

/// Add a UpdateMetadata IR step to the output trace
void OutputTrace::updateMetadata(shared_ptr<Command> cmd,
                                 shared_ptr<RefResult> ref,
                                 shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new UpdateMetadataRecord(getCommandID(cmd), ref, version));
}

/// Add a UpdateContent IR step to the output trace
void OutputTrace::updateContent(shared_ptr<Command> cmd,
                                shared_ptr<RefResult> ref,
                                shared_ptr<Version> version) noexcept {
  _records.emplace_back(new UpdateContentRecord(getCommandID(cmd), ref, version));
}

/// Add a Launch IR step to the output trace
void OutputTrace::launch(shared_ptr<Command> cmd, shared_ptr<Command> child) noexcept {
  // Add the launched command to the set of commands
  command_id_t child_id = addCommand(child);
  _records.emplace_back(new CommandRecord(child_id, child));

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

// Directory version types
CEREAL_REGISTER_TYPE(AddEntry);
CEREAL_REGISTER_TYPE(RemoveEntry);
CEREAL_REGISTER_TYPE(CreatedDir);
CEREAL_REGISTER_TYPE(ListedDir);
