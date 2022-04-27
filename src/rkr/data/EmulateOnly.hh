#include <memory>

#include "data/IRSink.hh"
#include "data/IRSource.hh"

class Command;
class ContentVersion;
class MetadataVersion;
class Ref;

template <class Next>
class EmulateOnly : public IRSink {
 public:
  EmulateOnly(Next& next) noexcept : _next(next) {}

  /// Called when starting a trace. The root command is passed in.
  virtual void start(const std::shared_ptr<Command>& c) noexcept { _next.start(c); }

  /// Called when the trace is finished
  virtual void finish() noexcept { _next.finish(); }

  /// Handle a SpecialRef IR step
  virtual void specialRef(const IRSource& source,
                          const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept {
    if (command->canEmulate()) _next.specialRef(source, command, entity, output);
  }

  /// Handle a PipeRef IR step
  virtual void pipeRef(const IRSource& source,
                       const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept {
    if (command->canEmulate()) _next.pipeRef(source, command, read_end, write_end);
  }

  /// Handle a FileRef IR step
  virtual void fileRef(const IRSource& source,
                       const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept {
    if (command->canEmulate()) _next.fileRef(source, command, mode, output);
  }

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const IRSource& source,
                          const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept {
    if (command->canEmulate()) _next.symlinkRef(source, command, target, output);
  }

  /// Handle a DirRef IR step
  virtual void dirRef(const IRSource& source,
                      const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept {
    if (command->canEmulate()) _next.dirRef(source, command, mode, output);
  }

  /// Handle a PathRef IR step
  virtual void pathRef(const IRSource& source,
                       const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept {
    if (command->canEmulate()) _next.pathRef(source, command, base, path, flags, output);
  }

  /// Handle a UsingRef IR step
  virtual void usingRef(const IRSource& source,
                        const std::shared_ptr<Command>& command,
                        Ref::ID ref) noexcept {
    if (command->canEmulate()) _next.usingRef(source, command, ref);
  }

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const IRSource& source,
                           const std::shared_ptr<Command>& command,
                           Ref::ID ref) noexcept {
    if (command->canEmulate()) _next.doneWithRef(source, command, ref);
  }

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const IRSource& source,
                           const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept {
    if (command->canEmulate()) _next.compareRefs(source, command, ref1, ref2, type);
  }

  /// Handle an ExpectResult IR step
  virtual void expectResult(const IRSource& source,
                            const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int8_t expected) noexcept {
    if (command->canEmulate()) _next.expectResult(source, command, scenario, ref, expected);
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const IRSource& source,
                             const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion version) noexcept {
    if (command->canEmulate()) _next.matchMetadata(source, command, scenario, ref, version);
  }

  /// Handel a MatchContent IR step
  virtual void matchContent(const IRSource& source,
                            const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept {
    if (command->canEmulate()) _next.matchContent(source, command, scenario, ref, version);
  }

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const IRSource& source,
                              const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion version) noexcept {
    if (command->canEmulate()) _next.updateMetadata(source, command, ref, version);
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(const IRSource& source,
                             const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept {
    if (command->canEmulate()) _next.updateContent(source, command, ref, version);
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const IRSource& source,
                        const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept {
    if (command->canEmulate()) _next.addEntry(source, command, dir, name, target);
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const IRSource& source,
                           const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept {
    if (command->canEmulate()) _next.removeEntry(source, command, dir, name, target);
  }

  /// Handle a Launch IR step
  virtual void launch(const IRSource& source,
                      const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept {
    if (command->canEmulate()) _next.launch(source, command, child, refs);
  }

  /// Handle a Join IR step
  virtual void join(const IRSource& source,
                    const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept {
    if (command->canEmulate()) _next.join(source, command, child, exit_status);
  }

  /// Handle an Exit IR step
  virtual void exit(const IRSource& source,
                    const std::shared_ptr<Command>& command,
                    int exit_status) noexcept {
    if (command->canEmulate()) _next.exit(source, command, exit_status);
  }

 private:
  Next& _next;
};
