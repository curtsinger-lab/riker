#pragma once

#include <cstddef>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>

#include "artifacts/Artifact.hh"
#include "runtime/Ref.hh"
#include "runtime/VersionState.hh"

namespace fs = std::filesystem;

class Build;
class Command;
class ContentVersion;
class MetadataVersion;
class ArtifactVersion;
class FileVersion;

class SocketArtifact : public Artifact {
 public:
  /**
   * Create a new SocketArtifact with no initial state. This is useful for modeled artifacts that
   * do not exist on the filesystem. This constructor should be followed-up with calls to
   * updateMetadata and updateContent to set the initial state.
   */
  SocketArtifact() noexcept : Artifact() {}

  /**
   * Create a SocketArtifact that represents an existing socket..
   */
  SocketArtifact(MetadataVersion mv, std::shared_ptr<FileVersion> sv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get a printable name for this artifact type
  virtual std::string getTypeName() const noexcept override { return "Socket"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /// Revert this artifact to its committed state
  virtual void rollback() noexcept override;

  /************ Path Operations ************/

  /// Commit a link to this artifact at the given path
  virtual void commitLink(std::shared_ptr<DirEntry> entry) noexcept override;

  /// Commit an unlink of this artifact at the given path
  virtual void commitUnlink(std::shared_ptr<DirEntry> entry) noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const IRSource& source,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build,
                         const IRSource& source,
                         const std::shared_ptr<Command>& c,
                         Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build,
                           const IRSource& source,
                           const std::shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A trace command just wrote to this artifact
  virtual void afterWrite(Build& build,
                          const IRSource& source,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

  /************ Content Operations ************/

  /// Get this artifact's current content
  virtual std::shared_ptr<ContentVersion> getContent(
      const std::shared_ptr<Command>& c) noexcept override;

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            std::shared_ptr<ContentVersion> expected) noexcept override;

  /// Set the content of this socket
  virtual void updateContent(const std::shared_ptr<Command>& c,
                             std::shared_ptr<ContentVersion> writing) noexcept override;
};
/************ Socket Operations ************/
template <>
struct fmt::formatter<SocketArtifact> : fmt::ostream_formatter {};
