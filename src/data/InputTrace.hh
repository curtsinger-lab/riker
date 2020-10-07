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
#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/RefResult.hh"

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
  InputTrace(fs::path buildfile, fs::path filename) noexcept :
      _buildfile(buildfile), _filename(filename) {}

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
  shared_ptr<Command> getCommand(Command::ID id) const noexcept { return _commands[id]; }

  /// Get a RefResult from its ID
  shared_ptr<RefResult> getRefResult(RefResult::ID id) noexcept {
    // Grow the vector if necessary
    if (_ref_results.size() <= id) _ref_results.resize(id + 1);

    // If the referenced entry is unset, initialize it
    if (!_ref_results[id]) _ref_results[id] = make_shared<RefResult>();

    // Return the refresult
    return _ref_results[id];
  }

 private:
  /// Send a default trace to a trace handler
  void sendDefault(TraceHandler& handler) noexcept;

 private:
  /// The path to the file that should be executed at the root of the build
  fs::path _buildfile;

  /// The path to the loaded trace
  fs::path _filename;

  /// The map from command IDs to command instances. Startup steps run in command 0
  vector<shared_ptr<Command>> _commands = {nullptr};

  /// The map from RefResult IDs to instances
  vector<shared_ptr<RefResult>> _ref_results;
};
