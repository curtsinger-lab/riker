#pragma once

#include <cstddef>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <tuple>

#include "artifacts/Artifact.hh"
#include "runtime/Ref.hh"

namespace fs = std::filesystem;

class AccessFlags;
class BaseDirVersion;
class Build;
class Command;
class ContentVersion;
class DirListVersion;
class DirVersion;
class MetadataVersion;

class DirArtifact final : public Artifact {
 public:
  /// Create a DirArtifact to represent a directory created by the given command
  DirArtifact(std::shared_ptr<Command> creator) noexcept;

  /// Create a DirArtifact with existing committed metadata and content
  DirArtifact(std::shared_ptr<MetadataVersion> mv, std::shared_ptr<BaseDirVersion> dv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "Dir"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override;

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept override;

  /// Commit all entries in this directory
  void commitAll() noexcept;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /************ Path Operations ************/

  /// Commit a link to this artifact at the given path
  virtual void commitLink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept override;

  /// Commit an unlink of this artifact at the given path
  virtual void commitUnlink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build,
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

  /************ Directory Operations ************/

  /// Add a directory entry to this artifact
  virtual void addEntry(const std::shared_ptr<Command>& c,
                        fs::path entry,
                        std::shared_ptr<Artifact> target) noexcept override;

  /// Remove a directory entry from this artifact
  virtual void removeEntry(const std::shared_ptr<Command>& c,
                           fs::path entry,
                           std::shared_ptr<Artifact> target) noexcept override;

  // Un-hide the shorthand version of resolve()
  using Artifact::resolve;

  virtual Ref resolve(const std::shared_ptr<Command>& c,
                      std::shared_ptr<Artifact> prev,
                      fs::path::iterator current,
                      fs::path::iterator end,
                      AccessFlags flags,
                      size_t symlink_limit) noexcept override;

 protected:
  /// Get the base version for this directory artifact, which may or may not be committed
  const std::shared_ptr<BaseDirVersion>& getBaseVersion() const noexcept;

 private:
  /// A link is a tuple of the artifact (possibly nullptr), the version that linked/unlinked the
  /// artifact, and the command responsible for the latest state
  using Link = std::tuple<std::shared_ptr<Artifact>,    // The linked artifact, or null if unlinked
                          std::shared_ptr<DirVersion>,  // The version responsible for the link
                          std::shared_ptr<Command>>;    // The command that wrote this version

  /// A map from entry names to links in this directory. Includes all links, committed or not.
  std::map<std::string, Link> _entries;

  /// A map of entries that are committed to the filesystem
  std::map<std::string, Link> _committed_entries;

  /// The base directory version is the backstop for all resolution queries. The base version can
  /// only be uncommitted if it is an empty directory.
  std::shared_ptr<BaseDirVersion> _uncommitted_base_version;

  /// The committed base directory version is committed ot the filesystem
  std::shared_ptr<BaseDirVersion> _committed_base_version;

  /// The command that created this directory, or nullptr
  std::shared_ptr<Command> _creator;
};
