#include "Record.hh"

#include "artifacts/PipeArtifact.hh"
#include "data/AccessFlags.hh"
#include "data/InputTrace.hh"
#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "versions/DirListVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

// Record types
CEREAL_REGISTER_TYPE(CommandRecord);
CEREAL_REGISTER_TYPE(SpecialRefRecord);
CEREAL_REGISTER_TYPE(PipeRefRecord);
CEREAL_REGISTER_TYPE(FileRefRecord);
CEREAL_REGISTER_TYPE(SymlinkRefRecord);
CEREAL_REGISTER_TYPE(DirRefRecord);
CEREAL_REGISTER_TYPE(PathRefRecord);
CEREAL_REGISTER_TYPE(OpenRecord);
CEREAL_REGISTER_TYPE(CloseRecord);
CEREAL_REGISTER_TYPE(CompareRefsRecord);
CEREAL_REGISTER_TYPE(ExpectResultRecord);
CEREAL_REGISTER_TYPE(MatchMetadataRecord);
CEREAL_REGISTER_TYPE(MatchContentRecord);
CEREAL_REGISTER_TYPE(UpdateMetadataRecord);
CEREAL_REGISTER_TYPE(UpdateContentRecord);
CEREAL_REGISTER_TYPE(AddEntryRecord);
CEREAL_REGISTER_TYPE(RemoveEntryRecord);
CEREAL_REGISTER_TYPE(LaunchRecord);
CEREAL_REGISTER_TYPE(JoinRecord);
CEREAL_REGISTER_TYPE(ExitRecord);
CEREAL_REGISTER_TYPE(EndRecord);

// Versions
CEREAL_REGISTER_TYPE(MetadataVersion);
CEREAL_REGISTER_TYPE(FileVersion);
CEREAL_REGISTER_TYPE(SymlinkVersion);
CEREAL_REGISTER_TYPE(DirListVersion);
CEREAL_REGISTER_TYPE(PipeWriteVersion);
CEREAL_REGISTER_TYPE(PipeReadVersion);

// Read a command from an input trace
void CommandRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
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

// Send a SpecialRef IR step from an input trace to a trace handler
void SpecialRefRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.specialRef(input.getCommand(_cmd), _entity, input.getRefResult(_output));
}

// Send a PipeRef IR step from an input trace to a trace handler
void PipeRefRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.pipeRef(input.getCommand(_cmd), input.getRefResult(_read_end),
                  input.getRefResult(_write_end));
}

// Send a FileRef IR step from an input trace to a trace handler
void FileRefRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.fileRef(input.getCommand(_cmd), _mode, input.getRefResult(_output));
}

// Send a SymlinkRef IR step from an input trace to a trace handler
void SymlinkRefRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.symlinkRef(input.getCommand(_cmd), _target, input.getRefResult(_output));
}

// Sedn a DirRef IR step from an input trace to a trace handler
void DirRefRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.dirRef(input.getCommand(_cmd), _mode, input.getRefResult(_output));
}

// Send a PathRef IR step from an input trace to a trace handler
void PathRefRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.pathRef(input.getCommand(_cmd), input.getRefResult(_base), _path, _flags,
                  input.getRefResult(_output));
}

// Send an Open IR step from an input trace to a trace handler
void OpenRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.open(input.getCommand(_cmd), input.getRefResult(_ref));
}

// Send a Close IR step from an input trace to a trace handler
void CloseRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.close(input.getCommand(_cmd), input.getRefResult(_ref));
}

// Send a CompareRefs IR step from an input trace to a trace handler
void CompareRefsRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.compareRefs(input.getCommand(_cmd), input.getRefResult(_ref1), input.getRefResult(_ref2),
                      _type);
}

// Send an ExpectResult IR step from an input trace to a trace handler
void ExpectResultRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.expectResult(input.getCommand(_cmd), _scenario, input.getRefResult(_ref), _expected);
}

// Send a MatchMetadata IR step from an input trace to a trace handler
void MatchMetadataRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.matchMetadata(input.getCommand(_cmd), _scenario, input.getRefResult(_ref), _version);
}

// Send a MatchContent IR step from an input trace to a trace handler
void MatchContentRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.matchContent(input.getCommand(_cmd), _scenario, input.getRefResult(_ref), _version);
}

// Send an UpdateMetadata IR step from an input trace to a trace handler
void UpdateMetadataRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.updateMetadata(input.getCommand(_cmd), input.getRefResult(_ref), _version);
}

// Send an UpdateContent IR step from an input trace to a trace handler
void UpdateContentRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.updateContent(input.getCommand(_cmd), input.getRefResult(_ref), _version);
}

// Send an AddEntry IR step from an input trace to a trace handler
void AddEntryRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.addEntry(input.getCommand(_cmd), input.getRefResult(_dir), _name,
                   input.getRefResult(_target));
}

// Send a RemoveEntry IR step from an input trace to a trace handler
void RemoveEntryRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.removeEntry(input.getCommand(_cmd), input.getRefResult(_dir), _name,
                      input.getRefResult(_target));
}

// Send a Launch IR step from an input trace to a trace handler
void LaunchRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.launch(input.getCommand(_cmd), input.getCommand(_child));
}

// Send a Join IR step from an input trace to a trace handler
void JoinRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.join(input.getCommand(_cmd), input.getCommand(_child), _exit_status);
}

// Send an Exit IR step from an input trace to a trace handler
void ExitRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {
  handler.exit(input.getCommand(_cmd), _exit_status);
}

// Handle a record that marks the end of an input trace
void EndRecord::handle(InputTrace& input, TraceHandler& handler) noexcept {}
