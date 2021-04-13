#pragma once

#include <memory>
#include <string>
#include <vector>

#include "data/IRSink.hh"
#include "data/IRSource.hh"

class DefaultTrace : public IRSource {
 public:
  /// Create a source for a default starting trace
  DefaultTrace(std::vector<std::string> args = {}) noexcept;

  /// Send a stream of IR steps to the given handler
  virtual void sendTo(IRSink& handler) noexcept override;

  /// Get the root command from this trace
  std::shared_ptr<Command> getRootCommand() const noexcept { return _root_command; }

 private:
  /// The root command for this default trace
  std::shared_ptr<Command> _root_command;

  /// Any special arguments passed to the default trace (used for testing only)
  std::vector<std::string> _args;
};
