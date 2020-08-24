#pragma once

#include <cstdio>
#include <memory>

#include "util/serializer.hh"

using std::shared_ptr;

class Command;

class RefResult {
 public:
  /// Default constructor for deserialization
  RefResult() noexcept = default;

  RefResult(shared_ptr<Command> cmd, size_t index) noexcept : _cmd(cmd), _index(index) {}

  shared_ptr<Command> getCommand() const noexcept { return _cmd; }

  size_t getIndex() const noexcept { return _index; }

 private:
  shared_ptr<Command> _cmd;
  size_t _index;

  SERIALIZE(_cmd, _index);
};