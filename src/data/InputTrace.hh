#pragma once

#include <filesystem>
#include <fstream>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>
#include <vector>

#include <cereal/archives/binary.hpp>

#include "data/Record.hh"
#include "data/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"

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
class InputTrace {
 public:
  /// Load an input trace from a given path, or produce a default starting trace if no trace exists
  InputTrace(vector<string> args, fs::path filename) noexcept : _args(args), _filename(filename) {
    // Add the null command to the command map
    _commands.emplace_back(Command::getNullCommand());
  }

  // Disallow copy
  InputTrace(const InputTrace&) = delete;
  InputTrace& operator=(const InputTrace&) = delete;

  // Allow move
  InputTrace(InputTrace&&) = default;
  InputTrace& operator=(InputTrace&&) = default;

  /// Send the loaded trace to a trace handler
  void sendTo(TraceHandler& handler) noexcept;

  /// Send the loaded trace to a trace handler
  void sendTo(TraceHandler&& handler) noexcept { sendTo(handler); }

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

  /// Get the commands loaded from this input trace
  const vector<shared_ptr<Command>>& getCommands() const noexcept { return _commands; }

  /// Get the root command from this trace
  const shared_ptr<Command>& getRootCommand() const noexcept { return _commands[1]; }

 private:
  /// Send a default trace to a trace handler
  void sendDefault(TraceHandler& handler) noexcept;

 private:
  /// Any extra arguments a user may supply to a buildfile
  vector<string> _args;

  /// The path to the loaded trace
  fs::path _filename;

  /// The map from command IDs to command instances
  vector<shared_ptr<Command>> _commands;
};
