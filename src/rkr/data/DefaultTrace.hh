#pragma once

#include <memory>
#include <string>
#include <vector>

#include "data/IRSource.hh"

class Command;
class IRSink;

class DefaultTrace : public IRSource {
 public:
  /// Create a source for a default starting trace
  DefaultTrace(std::vector<std::string> args = {}) noexcept;

  /// Send a stream of IR steps to the given handler
  void sendTo(IRSink& handler) noexcept;

  /// Send a stream of IR steps to an r-value reference handler
  void sendTo(IRSink&& handler) noexcept { sendTo(handler); }

  /// Send a stream of IR steps to the given handler based on given command
  void sendToHelp(IRSink& handler, std::string command) noexcept;

  /// Send a stream of IR steps to an r-value reference handler on given command
  void sendToHelp(IRSink&& handler, std::string command) noexcept { sendToHelp(handler, command); }

  /// Get the root command from this trace
  std::shared_ptr<Command> getRootCommand() const noexcept { return _root_command; }

  /// The default trace is never an executing IRSource
  virtual bool isExecuting() const override { return false; }

 private:
  /// The root command for this default trace
  std::shared_ptr<Command> _root_command;

  /// Any special arguments passed to the default trace (used for testing only)
  std::vector<std::string> _args;
};
