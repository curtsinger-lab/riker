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
  virtual void addCommand(Command::ID id, std::shared_ptr<Command> c) noexcept = 0;

  /// Get a command instance from its ID
  virtual const std::shared_ptr<Command>& getCommand(Command::ID id) const noexcept = 0;

  /// Identify a metadata version with a given ID
  virtual void addMetadataVersion(MetadataVersion::ID id,
                                  std::shared_ptr<MetadataVersion> mv) noexcept = 0;

  /// Get a metadata version from its ID
  virtual const std::shared_ptr<MetadataVersion>& getMetadataVersion(
      MetadataVersion::ID id) const noexcept = 0;

  /// Identify a content version with a given ID
  virtual void addContentVersion(ContentVersion::ID id,
                                 std::shared_ptr<ContentVersion> cv) noexcept = 0;

  /// Get a content version from its ID
  virtual const std::shared_ptr<ContentVersion>& getContentVersion(
      ContentVersion::ID id) const noexcept = 0;
};
