#pragma once

#include <filesystem>
#include <memory>

#include "core/AccessFlags.hh"
#include "core/SpecialRefs.hh"

class Command;
class MetadataVersion;
class RefResult;
class Version;

using std::shared_ptr;

namespace fs = std::filesystem;

class TraceHandler {
 public:
  /// Called when the trace is finished
  virtual void finish() noexcept {}

  /// Handle a SpecialRef IR step
  virtual void specialRef(shared_ptr<Command> command,
                          SpecialRef entity,
                          shared_ptr<RefResult> output) noexcept = 0;

  /// Handle a PipeRef IR step
  virtual void pipeRef(shared_ptr<Command> command,
                       shared_ptr<RefResult> read_end,
                       shared_ptr<RefResult> write_end) noexcept = 0;

  /// Handle a FileRef IR step
  virtual void fileRef(shared_ptr<Command> command,
                       mode_t mode,
                       shared_ptr<RefResult> output) noexcept = 0;

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(shared_ptr<Command> command,
                          fs::path target,
                          shared_ptr<RefResult> output) noexcept = 0;

  /// Handle a DirRef IR step
  virtual void dirRef(shared_ptr<Command> command,
                      mode_t mode,
                      shared_ptr<RefResult> output) noexcept = 0;

  /// Handle a PathRef IR step
  virtual void pathRef(shared_ptr<Command> command,
                       shared_ptr<RefResult> base,
                       fs::path path,
                       AccessFlags flags,
                       shared_ptr<RefResult> output) noexcept = 0;

  /// Handle an ExpectResult IR step
  virtual void expectResult(shared_ptr<Command> command,
                            shared_ptr<RefResult> ref,
                            int expected) noexcept = 0;

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(shared_ptr<Command> command,
                             shared_ptr<RefResult> ref,
                             shared_ptr<MetadataVersion> version) noexcept = 0;

  /// Handel a MatchContent IR step
  virtual void matchContent(shared_ptr<Command> command,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> version) noexcept = 0;

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(shared_ptr<Command> command,
                              shared_ptr<RefResult> ref,
                              shared_ptr<MetadataVersion> version) noexcept = 0;

  /// Handle an UpdateContent IR step
  virtual void updateContent(shared_ptr<Command> command,
                             shared_ptr<RefResult> ref,
                             shared_ptr<Version> version) noexcept = 0;

  /// Handle an AddEntry IR step
  virtual void addEntry(shared_ptr<Command> command,
                        shared_ptr<RefResult> dir,
                        fs::path name,
                        shared_ptr<RefResult> target) noexcept = 0;

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(shared_ptr<Command> command,
                           shared_ptr<RefResult> dir,
                           fs::path name,
                           shared_ptr<RefResult> target) noexcept = 0;

  /// Handle a Launch IR step
  virtual void launch(shared_ptr<Command> command, shared_ptr<Command> child) noexcept = 0;

  /// Handle a Join IR step
  virtual void join(shared_ptr<Command> command,
                    shared_ptr<Command> child,
                    int exit_status) noexcept = 0;

  /// Handle an Exit IR step
  virtual void exit(shared_ptr<Command> command, int exit_status) noexcept = 0;
};
