#pragma once

#include <memory>
#include <ostream>

#include "core/SpecialRefs.hh"
#include "core/TraceHandler.hh"

using std::ostream;
using std::shared_ptr;

class Command;
class RefResult;

class TracePrinter : public TraceHandler {
 public:
  /// Create a trace printer that writes to a provided ostream
  TracePrinter(ostream& out) : _out(out) {}

  /// Create a trace printer that writes to a provided ostream (rvalue reference form)
  TracePrinter(ostream&& out) : _out(out) {}

  virtual void specialRef(shared_ptr<Command> c,
                          SpecialRef entity,
                          shared_ptr<RefResult> output) noexcept override;

  virtual void pipeRef(shared_ptr<Command> c,
                       shared_ptr<RefResult> read_end,
                       shared_ptr<RefResult> write_end) noexcept override;

  virtual void fileRef(shared_ptr<Command> c,
                       mode_t mode,
                       shared_ptr<RefResult> output) noexcept override;

  virtual void symlinkRef(shared_ptr<Command> c,
                          fs::path target,
                          shared_ptr<RefResult> output) noexcept override;

  virtual void dirRef(shared_ptr<Command> c,
                      mode_t mode,
                      shared_ptr<RefResult> output) noexcept override;

  virtual void pathRef(shared_ptr<Command> c,
                       shared_ptr<RefResult> base,
                       fs::path path,
                       AccessFlags flags,
                       shared_ptr<RefResult> output) noexcept override;

  virtual void expectResult(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            int expected) noexcept override;

  virtual void matchMetadata(shared_ptr<Command> c,
                             shared_ptr<RefResult> ref,
                             shared_ptr<MetadataVersion> expected) noexcept override;

  virtual void matchContent(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> expected) noexcept override;

  virtual void updateMetadata(shared_ptr<Command> c,
                              shared_ptr<RefResult>,
                              shared_ptr<MetadataVersion> written) noexcept override;

  virtual void updateContent(shared_ptr<Command> c,
                             shared_ptr<RefResult> ref,
                             shared_ptr<Version> written) noexcept override;

  virtual void launch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept override;

  virtual void join(shared_ptr<Command> c,
                    shared_ptr<Command> child,
                    int exit_status) noexcept override;

  virtual void exit(shared_ptr<Command> c, int exit_status) noexcept override;

 private:
  ostream& _out;
};
