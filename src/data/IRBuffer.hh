#pragma once

#include <filesystem>
#include <functional>
#include <list>
#include <memory>

#include "data/IRSink.hh"
#include "data/IRSource.hh"
#include "util/log.hh"

class Command;
class MetadataVersion;

namespace fs = std::filesystem;

class IRBuffer : public IRSource, public IRSink {
 public:
  /**** IRSource Methods ****/

  /// Send the stored IR trace to a sink
  virtual void sendTo(IRSink& handler) noexcept override {
    // Set the buffer to draining mode
    _draining = true;

    // Send steps while the list is not empty
    while (!_steps.empty()) {
      _steps.front()(handler);
      _steps.pop_front();
    }

    // Now the buffer can fill again
    _draining = false;
  }

  /**** IRSink Methods ****/

  /// Start a build with the given root command
  virtual void start(const std::shared_ptr<Command>& c) noexcept override {
    ASSERT(c) << "IRBuffer::start() called with null root command";
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _root_command = c;
    _steps.emplace_back([=](IRSink& handler) { handler.start(c); });
  }

  /// Called when the trace is finished
  virtual void finish() noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.finish(); });
  }

  /// Handle a SpecialRef IR step
  virtual void specialRef(const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.specialRef(command, entity, output); });
  }

  /// Handle a PipeRef IR step
  virtual void pipeRef(const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.pipeRef(command, read_end, write_end); });
  }

  /// Handle a FileRef IR step
  virtual void fileRef(const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.fileRef(command, mode, output); });
  }

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.symlinkRef(command, target, output); });
  }

  /// Handle a DirRef IR step
  virtual void dirRef(const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.dirRef(command, mode, output); });
  }

  /// Handle a PathRef IR step
  virtual void pathRef(const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back(
        [=](IRSink& handler) { handler.pathRef(command, base, path, flags, output); });
  }

  /// Handle a UsingRef IR step
  virtual void usingRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.usingRef(command, ref); });
  }

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.doneWithRef(command, ref); });
  }

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.compareRefs(command, ref1, ref2, type); });
  }

  /// Handle an ExpectResult IR step
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back(
        [=](IRSink& handler) { handler.expectResult(command, scenario, ref, expected); });
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             std::shared_ptr<MetadataVersion> version) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back(
        [=](IRSink& handler) { handler.matchMetadata(command, scenario, ref, version); });
  }

  /// Handel a MatchContent IR step
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back(
        [=](IRSink& handler) { handler.matchContent(command, scenario, ref, version); });
  }

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              std::shared_ptr<MetadataVersion> version) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.updateMetadata(command, ref, version); });
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.updateContent(command, ref, version); });
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.addEntry(command, dir, name, target); });
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.removeEntry(command, dir, name, target); });
  }

  /// Handle a Launch IR step
  virtual void launch(const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.launch(command, child, refs); });
  }

  /// Handle a Join IR step
  virtual void join(const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.join(command, child, exit_status); });
  }

  /// Handle an Exit IR step
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override {
    ASSERT(!_draining) << "Cannot add steps to a buffer that is draining";
    _steps.emplace_back([=](IRSink& handler) { handler.exit(command, exit_status); });
  }

 private:
  /// Is the buffer currently draining?
  bool _draining = false;

  /// Keep track of the root command
  std::shared_ptr<Command> _root_command;

  /// Keep a list of IR steps as callbacks that take a sink as input
  std::list<std::function<void(IRSink&)>> _steps;
};