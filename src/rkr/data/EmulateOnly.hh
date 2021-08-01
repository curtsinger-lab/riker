#include <memory>

#include "data/IRSink.hh"

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
  virtual void specialRef(const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept {
    _next.specialRef(command, entity, output);
  }

  /// Handle a PipeRef IR step
  virtual void pipeRef(const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept {
    if (!command->mustRun()) _next.pipeRef(command, read_end, write_end);
  }

  /// Handle a FileRef IR step
  virtual void fileRef(const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept {
    _next.fileRef(command, mode, output);
  }

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept {
    _next.symlinkRef(command, target, output);
  }

  /// Handle a DirRef IR step
  virtual void dirRef(const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept {
    _next.dirRef(command, mode, output);
  }

  /// Handle a PathRef IR step
  virtual void pathRef(const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept {
    _next.pathRef(command, base, path, flags, output);
  }

  /// Handle a UsingRef IR step
  virtual void usingRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept {
    _next.usingRef(command, ref);
  }

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept {
    _next.doneWithRef(command, ref);
  }

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept {
    _next.compareRefs(command, ref1, ref2, type);
  }

  /// Handle an ExpectResult IR step
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept {
    _next.expectResult(command, scenario, ref, expected);
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion version) noexcept {
    _next.matchMetadata(command, scenario, ref, version);
  }

  /// Handel a MatchContent IR step
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept {
    _next.matchContent(command, scenario, ref, version);
  }

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion version) noexcept {
    _next.updateMetadata(command, ref, version);
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept {
    _next.updateContent(command, ref, version);
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept {
    _next.addEntry(command, dir, name, target);
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept {
    _next.removeEntry(command, dir, name, target);
  }

  /// Handle a Launch IR step
  virtual void launch(const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept {
    _next.launch(command, child, refs);
  }

  /// Handle a Join IR step
  virtual void join(const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept {
    _next.join(command, child, exit_status);
  }

  /// Handle an Exit IR step
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept {
    _next.exit(command, exit_status);
  }

 private:
  Next& _next;
};
