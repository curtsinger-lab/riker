#pragma once

// IWYU pragma: no_include <ext/alloc_traits.h>

#include <filesystem>
#include <fstream>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <cereal/archives/binary.hpp>

#include "data/IRSink.hh"
#include "data/IRSource.hh"
#include "runtime/Command.hh"

using std::ifstream;
using std::list;
using std::make_shared;
using std::map;
using std::ofstream;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;

namespace fs = std::filesystem;

/**
 * An input trace is a build trace loaded from disk
 */
class InputTrace : public IRSource {
 private:
  InputTrace(string filename, vector<string> args = {});

 public:
  static unique_ptr<IRSource> load(string filename, vector<string> args = {}) noexcept;

  // Disallow copy
  InputTrace(const InputTrace&) = delete;
  InputTrace& operator=(const InputTrace&) = delete;

  /// Send the loaded trace to a trace handler
  virtual void sendTo(IRSink& handler) noexcept override;

  /// Send the loaded trace to an r-value trace handler
  virtual void sendTo(IRSink&& handler) noexcept override { sendTo(handler); }

  /// Add a command with a known ID to this input trace. If the command ID has already been loaded,
  /// the original instance will be used and not the new one.
  void addCommand(Command::ID id, shared_ptr<Command> cmd) noexcept {
    // Grow the commands vector if necessary
    if (_commands.size() <= id) _commands.resize(id + 1);

    // If the referenced entry is unset, save the provided cmd
    if (!_commands[id]) _commands[id] = cmd;
  }

  /// Get a command from its ID
  const shared_ptr<Command>& getCommand(Command::ID id) const noexcept { return _commands[id]; }

  /// Check if this input trace has a command with a given ID
  bool hasCommand(Command::ID id) const noexcept { return id >= 0 && _commands.size() > id; }

 private:
  /// The input stream this trace is read from
  ifstream _input;

  /// The binary archive that decodes the loaded trace
  cereal::BinaryInputArchive _archive;

  /// Any extra arguments a user may supply to a buildfile
  vector<string> _args;

  /// The map from command IDs to command instances
  vector<shared_ptr<Command>> _commands;
};
