#pragma once

#include <string>
#include <vector>

#include "data/IRSource.hh"

using std::string;
using std::vector;

class DefaultTrace : public IRSource {
 public:
  /// Create a source for a default starting trace
  DefaultTrace(vector<string> args = {}) noexcept : _args(args) {}

  /// Send a stream of IR steps to the given handler
  virtual void sendTo(IRSink& handler) noexcept override;

  /// Accept r-value reference to a trace handler
  virtual void sendTo(IRSink&& handler) noexcept override { sendTo(handler); }

 private:
  vector<string> _args;
};
