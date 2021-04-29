#pragma once

#include <memory>

#include "runtime/Command.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

/**
 * An IRLoader is a class that is responsible for loading IR from a serialized representation. The
 * loader is required to maintain a mapping from IDs to instances for commands, metadata versions,
 * and content versions.
 */
class IRLoader {
 public:
  /// Identify a command with a given ID
  virtual void addCommand(Command::ID id, std::shared_ptr<Command> c) noexcept {
    // Grow the commands vector if necessary
    if (_commands.size() <= id) _commands.resize(id + 1);

    // If the referenced entry is unset, save the provided cmd
    if (!_commands[id]) _commands[id] = c;
  }

  /// Get a command instance from its ID
  virtual const std::shared_ptr<Command>& getCommand(Command::ID id) const noexcept {
    return _commands[id];
  }

  /// Identify a metadata version with a given ID
  virtual void addMetadataVersion(MetadataVersion::ID id,
                                  std::shared_ptr<MetadataVersion> mv) noexcept {
    // Grow the vector if necessary
    if (_metadata_versions.size() <= id) _metadata_versions.resize(id + 1);

    // If the referenced entry is not set, save the provided version
    if (!_metadata_versions[id]) _metadata_versions[id] = mv;
  }

  /// Get a metadata version from its ID
  virtual const std::shared_ptr<MetadataVersion>& getMetadataVersion(
      MetadataVersion::ID id) const noexcept {
    return _metadata_versions[id];
  }

  /// Identify a content version with a given ID
  virtual void addContentVersion(ContentVersion::ID id,
                                 std::shared_ptr<ContentVersion> cv) noexcept {
    // Grow the vector if necessary
    if (_content_versions.size() <= id) _content_versions.resize(id + 1);

    // If the referenced entry is not set, save the provided version
    if (!_content_versions[id]) _content_versions[id] = cv;
  }

  /// Get a content version from its ID
  virtual const std::shared_ptr<ContentVersion>& getContentVersion(
      ContentVersion::ID id) const noexcept {
    return _content_versions[id];
  }

 private:
  /// The map from command IDs to command instances
  std::vector<std::shared_ptr<Command>> _commands;

  /// The map from metadata version IDs to instances
  std::vector<std::shared_ptr<MetadataVersion>> _metadata_versions;

  /// The map from content version IDs to instances
  std::vector<std::shared_ptr<ContentVersion>> _content_versions;
};
