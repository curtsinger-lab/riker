#include <memory>

#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"

using std::shared_ptr;

/**
 * This class processes a build trace that has already been completed, and adds new predicates to
 * check against the state left at the end of a build.
 */
class PostBuildChecker : public TraceHandler {
 public:
  PostBuildChecker(TraceHandler& output) : _output(output) {}

  /// Called when the trace is finished
  virtual void finish() noexcept override { _output.finish(); }

  /// Handle a SpecialRef IR step
  virtual void specialRef(const shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override {
    _output.specialRef(command, entity, output);
  }

  /// Handle a PipeRef IR step
  virtual void pipeRef(const shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override {
    _output.pipeRef(command, read_end, write_end);
  }

  /// Handle a FileRef IR step
  virtual void fileRef(const shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override {
    _output.fileRef(command, mode, output);
  }

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override {
    _output.symlinkRef(command, target, output);
  }

  /// Handle a DirRef IR step
  virtual void dirRef(const shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override {
    _output.dirRef(command, mode, output);
  }

  /// Handle a PathRef IR step
  virtual void pathRef(const shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override {
    _output.pathRef(command, base, path, flags, output);
  }

  /// Handle a UsingRef IR step
  virtual void usingRef(const shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    _output.usingRef(command, ref);
  }

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    _output.doneWithRef(command, ref);
  }

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override {
    // TODO: Update comparison predicate?
    _output.compareRefs(command, ref1, ref2, type);
  }

  /// Handle an ExpectResult IR step
  virtual void expectResult(const shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    if (scenario == Scenario::Build) {
      _output.expectResult(command, Scenario::Build, ref, expected);
      _output.expectResult(command, Scenario::PostBuild, ref,
                           command->currentRun()->getRef(ref)->getResultCode());
    }
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             shared_ptr<MetadataVersion> expected) noexcept override {
    if (scenario == Scenario::Build) {
      // Emit the predicate from the original build phase
      _output.matchMetadata(command, Scenario::Build, ref, expected);

      // Now also emit a predicate to check for the post-build state
      if (command->currentRun()->getRef(ref)->isResolved()) {
        _output.matchMetadata(command, Scenario::PostBuild, ref,
                              command->currentRun()->getRef(ref)->getArtifact()->peekMetadata());
      } else {
        // Do we need to make sure the reference is not resolved? Hasn't that already been done?
      }
    }
  }

  /// Handle a MatchContent IR step
  virtual void matchContent(const shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            shared_ptr<Version> expected) noexcept override {
    if (scenario == Scenario::Build) {
      // Emit the predicate from the original build phase
      _output.matchContent(command, Scenario::Build, ref, expected);

      // Now also emit a predicate to check for the post-build state
      if (command->currentRun()->getRef(ref)->isResolved()) {
        _output.matchContent(command, Scenario::PostBuild, ref,
                             command->currentRun()->getRef(ref)->getArtifact()->peekContent());
      } else {
        // Do we need to make sure the reference is not resolved? Hasn't that already been done?
      }
    }
  }

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const shared_ptr<Command>& command,
                              Ref::ID ref,
                              shared_ptr<MetadataVersion> version) noexcept override {
    _output.updateMetadata(command, ref, version);
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(const shared_ptr<Command>& command,
                             Ref::ID ref,
                             shared_ptr<Version> version) noexcept override {
    _output.updateContent(command, ref, version);
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept override {
    _output.addEntry(command, dir, name, target);
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const shared_ptr<Command>& command,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept override {
    _output.removeEntry(command, dir, name, target);
  }

  /// Handle a Launch IR step
  virtual void launch(const shared_ptr<Command>& command,
                      const shared_ptr<Command>& child,
                      list<tuple<Ref::ID, Ref::ID>> refs) noexcept override {
    _output.launch(command, child, refs);
  }

  /// Handle a Join IR step
  virtual void join(const shared_ptr<Command>& command,
                    const shared_ptr<Command>& child,
                    int exit_status) noexcept override {
    _output.join(command, child, exit_status);
  }

  /// Handle an Exit IR step
  virtual void exit(const shared_ptr<Command>& command, int exit_status) noexcept override {
    _output.exit(command, exit_status);
  }

 private:
  TraceHandler& _output;
};
