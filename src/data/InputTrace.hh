#pragma once

#include <filesystem>
#include <fstream>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <cereal/archives/binary.hpp>

#include "data/IRSink.hh"
#include "data/IRSource.hh"
#include "runtime/Command.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

/**
 * An input trace is a build trace loaded from disk
 */
class InputTrace : public IRSource {
 private:
  InputTrace(std::string filename, std::vector<std::string> args = {});

 public:
  /**
   * Load an IR trace from disk, or create a default trace if no previous trace exists.
   *
   * \returns a tuple of the root command from the loaded trace and the IRSource
   */
  static std::tuple<std::shared_ptr<Command>, std::unique_ptr<IRSource>> load(
      std::string filename,
      std::vector<std::string> args = {}) noexcept;

  // Disallow copy
  InputTrace(const InputTrace&) = delete;
  InputTrace& operator=(const InputTrace&) = delete;

  /// Send the loaded trace to a trace handler
  virtual void sendTo(IRSink& handler) noexcept override;

  /// Add a command with a known ID to this input trace. If the command ID has already been loaded,
  /// the original instance will be used and not the new one.
  void addCommand(Command::ID id, std::shared_ptr<Command> cmd) noexcept {
    // Grow the commands vector if necessary
    if (_commands.size() <= id) _commands.resize(id + 1);

    // If the referenced entry is unset, save the provided cmd
    if (!_commands[id]) _commands[id] = cmd;
  }

  /// Get a command from its ID
  const std::shared_ptr<Command>& getCommand(Command::ID id) const noexcept {
    return _commands[id];
  }

  /// Add a MetadataVersion with a known ID to this input trace
  void addMetadataVersion(MetadataVersion::ID id, std::shared_ptr<MetadataVersion> mv) noexcept {
    // Grow the vector if necessary
    if (_metadata_versions.size() <= id) _metadata_versions.resize(id + 1);

    // If the referenced entry is not set, save the provided version
    if (!_metadata_versions[id]) _metadata_versions[id] = mv;
  }

  /// Get a metadata version from its ID
  const std::shared_ptr<MetadataVersion>& getMetadataVersion(
      MetadataVersion::ID id) const noexcept {
    return _metadata_versions[id];
  }

  /// Get the root command for this trace
  std::shared_ptr<Command> getRootCommand() const noexcept { return _commands[0]; }

  /// Check if this input trace has a command with a given ID
  bool hasCommand(Command::ID id) const noexcept { return id >= 0 && _commands.size() > id; }

 private:
  /// The input stream this trace is read from
  std::ifstream _input;

  /// The binary archive that decodes the loaded trace
  cereal::BinaryInputArchive _archive;

  /// Any extra arguments a user may supply to a buildfile
  std::vector<std::string> _args;

  /// The map from command IDs to command instances
  std::vector<std::shared_ptr<Command>> _commands;

  /// The map from metadata version IDs to instances
  std::vector<std::shared_ptr<MetadataVersion>> _metadata_versions;
};
