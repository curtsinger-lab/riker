#pragma once

#include <memory>

class Command;
class IRSink;

class IRSource {
 public:
  /// Virtual destructor
  virtual ~IRSource() noexcept = default;

  /// Send a stream of IR steps to the given handler
  virtual void sendTo(IRSink& handler) noexcept = 0;

  /// Accept r-value reference to a trace handler
  void sendTo(IRSink&& handler) noexcept { return sendTo(handler); }
};