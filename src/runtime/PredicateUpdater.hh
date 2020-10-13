#include <memory>

#include "interfaces/TraceHandler.hh"
#include "runtime/RefResult.hh"

using std::shared_ptr;

class PredicateUpdater : public TraceHandler {
 public:
  PredicateUpdater(TraceHandler& output) : _output(output) {}

  /// Called when the trace is finished
  virtual void finish() noexcept override { _output.finish(); }

  /// Handle a SpecialRef IR step
  virtual void specialRef(shared_ptr<Command> command,
                          SpecialRef entity,
                          shared_ptr<RefResult> output) noexcept override {
    _output.specialRef(command, entity, output);
  }

  /// Handle a PipeRef IR step
  virtual void pipeRef(shared_ptr<Command> command,
                       shared_ptr<RefResult> read_end,
                       shared_ptr<RefResult> write_end) noexcept override {
    _output.pipeRef(command, read_end, write_end);
  }

  /// Handle a FileRef IR step
  virtual void fileRef(shared_ptr<Command> command,
                       mode_t mode,
                       shared_ptr<RefResult> output) noexcept override {
    _output.fileRef(command, mode, output);
  }

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(shared_ptr<Command> command,
                          fs::path target,
                          shared_ptr<RefResult> output) noexcept override {
    _output.symlinkRef(command, target, output);
  }

  /// Handle a DirRef IR step
  virtual void dirRef(shared_ptr<Command> command,
                      mode_t mode,
                      shared_ptr<RefResult> output) noexcept override {
    _output.dirRef(command, mode, output);
  }

  /// Handle a PathRef IR step
  virtual void pathRef(shared_ptr<Command> command,
                       shared_ptr<RefResult> base,
                       fs::path path,
                       AccessFlags flags,
                       shared_ptr<RefResult> output) noexcept override {
    _output.pathRef(command, base, path, flags, output);
  }

  /// Handle a CompareRefs IR step
  virtual void compareRefs(shared_ptr<Command> command,
                           shared_ptr<RefResult> ref1,
                           shared_ptr<RefResult> ref2,
                           RefComparison type) noexcept override {
    // TODO: Update comparison predicate?
    _output.compareRefs(command, ref1, ref2, type);
  }

  /// Handle an ExpectResult IR step
  virtual void expectResult(shared_ptr<Command> command,
                            shared_ptr<RefResult> ref,
                            int expected) noexcept override {
    _output.expectResult(command, ref, ref->getResultCode());
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(shared_ptr<Command> command,
                             shared_ptr<RefResult> ref,
                             shared_ptr<MetadataVersion> version) noexcept override {
    if (!ref->isResolved()) return;
    BuildObserver o;
    auto mv = ref->getArtifact()->getMetadata(o, command, InputType::Accessed);
    _output.matchMetadata(command, ref, mv);
  }

  /// Handle a MatchContent IR step
  virtual void matchContent(shared_ptr<Command> command,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> version) noexcept override {
    if (!ref->isResolved()) return;
    _output.matchContent(command, ref, version);
  }

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(shared_ptr<Command> command,
                              shared_ptr<RefResult> ref,
                              shared_ptr<MetadataVersion> version) noexcept override {
    if (!ref->isResolved()) return;
    _output.updateMetadata(command, ref, version);
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(shared_ptr<Command> command,
                             shared_ptr<RefResult> ref,
                             shared_ptr<Version> version) noexcept override {
    if (!ref->isResolved()) return;
    _output.updateContent(command, ref, version);
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(shared_ptr<Command> command,
                        shared_ptr<RefResult> dir,
                        fs::path name,
                        shared_ptr<RefResult> target) noexcept override {
    if (!dir->isResolved() || !target->isResolved()) return;
    _output.addEntry(command, dir, name, target);
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(shared_ptr<Command> command,
                           shared_ptr<RefResult> dir,
                           fs::path name,
                           shared_ptr<RefResult> target) noexcept override {
    if (!dir->isResolved() || !target->isResolved()) return;
    _output.removeEntry(command, dir, name, target);
  }

  /// Handle a Launch IR step
  virtual void launch(shared_ptr<Command> command, shared_ptr<Command> child) noexcept override {
    _output.launch(command, child);
  }

  /// Handle a Join IR step
  virtual void join(shared_ptr<Command> command,
                    shared_ptr<Command> child,
                    int exit_status) noexcept override {
    _output.join(command, child, exit_status);
  }

  /// Handle an Exit IR step
  virtual void exit(shared_ptr<Command> command, int exit_status) noexcept override {
    _output.exit(command, exit_status);
  }

 private:
  TraceHandler& _output;
};
