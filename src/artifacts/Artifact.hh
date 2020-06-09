#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "ui/options.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

class Command;
class ContentVersion;
class Env;
class MetadataVersion;
class Reference;
class Version;

/**
 * An artifact is a thin wrapper class around a sequence of artifact versions. The artifact
 * represents a single file, pipe, socket, etc. that is accessed and (potentially) modified
 * throughout its life. Artifact instances are not serialized, but are used during building to
 * ensure all operations on a given file, pipe, etc. refer to the latest versions of that
 * artifact.
 */
class Artifact : public std::enable_shared_from_this<Artifact> {
 public:
  /**
   * Create a new artifact. Only accessibly to this class and Env
   * \param env       This artifact is instantiated as part of this environment
   * \param committed Does the initial version of this artifact represent the filesystem state?
   * \param v         An initial version the new artifact should be seeded with
   */
  Artifact(Env& env, bool committed, shared_ptr<MetadataVersion> v);

  // Required virtual destructor
  virtual ~Artifact() = default;

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  /// Get the name of this artifact used for pretty-printing
  const string& getName() const { return _name; }

  /// Set the name of this artifact used for pretty-printing. Prefer shorter names.
  void setName(string newname) {
    if (newname.empty()) return;
    if (_name.empty() || newname.size() < _name.size()) _name = newname;
  }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const { return _versions; }

  /// Do we have a saved copy of this artifact that can be committed to the filesystem?
  virtual bool isSaved() const;

  /// Save a copy of this artifact's versions so it can be restored on a future build
  virtual void save(const shared_ptr<Reference>& ref);

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(const shared_ptr<Reference>& ref);

  /// Do we have a fingerprint of this artifact's versions that will allow us to check for a match?
  virtual bool hasFingerprint() const;

  /// Save a fingerprint of this artifact's versions so we can check for a match
  virtual void fingerprint(const shared_ptr<Reference>& ref);

  /// Check this artifact's final state against the filesystem and report any changes
  virtual void checkFinalState(const shared_ptr<Reference>& ref);

  /********** Metadata: All Artifact Types **********/

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getMetadataCreator() const { return _metadata_creator.lock(); }

  /// Get the reference used to create the latest metadata version
  shared_ptr<Reference> getMetadataReference() const { return _metadata_ref.lock(); }

  bool isMetadataAccessed() const { return _metadata_accessed; }

  shared_ptr<MetadataVersion> peekMetadata() const { return _metadata_version; }

  /**
   * Command c accesses the metadata for this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  shared_ptr<MetadataVersion> accessMetadata(const shared_ptr<Command>& c,
                                             const shared_ptr<Reference>& ref);

  /**
   * Command c sets the metadata of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's metadata is set to, or null if the version is on disk
   * \returns the newly-assigned metadata version
   */
  shared_ptr<MetadataVersion> setMetadata(const shared_ptr<Command>& c,
                                          const shared_ptr<Reference>& ref,
                                          const shared_ptr<MetadataVersion>& v = nullptr);

  /********** Content: Files and Pipes **********/

  /// Get the creator of the latest version of this artifact
  virtual shared_ptr<Command> getContentCreator() const {
    WARN << "Invalid reference to contents of artifact " << this;
    return nullptr;
  }

  // Get the reference used to create the latest content version
  virtual shared_ptr<Reference> getContentReference() const {
    WARN << "Invalid reference to contents of artifact " << this;
    return nullptr;
  }

  virtual bool isContentAccessed() const {
    WARN << "Invalid reference to contents of artifact " << this;
    return true;
  }

  virtual shared_ptr<ContentVersion> peekContent() const {
    WARN << "Invalid reference to contents of artifact " << this;
    return nullptr;
  }

  /**
   * Command c accesses the content of this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  virtual shared_ptr<ContentVersion> accessContents(const shared_ptr<Command>& c,
                                                    const shared_ptr<Reference>& ref) {
    WARN << "Invalid reference to contents of artifact " << this << " with reference " << ref;
    return nullptr;
  }

  /**
   * Command c sets the content of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's content is set to, or null if the version is on disk
   * \returns the newly-assigned content version
   */
  virtual shared_ptr<ContentVersion> setContents(const shared_ptr<Command>& c,
                                                 const shared_ptr<Reference>& ref,
                                                 const shared_ptr<ContentVersion>& v = nullptr) {
    WARN << "Invalid reference to contents of artifact " << this << " with reference " << ref;
    return nullptr;
  }

  /****** Utility Methods ******/

  /// Get the name of this artifact type
  virtual string getTypeName() const = 0;

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    o << "[" << a.getTypeName();
    auto name = a.getName();
    if (!name.empty()) o << " " << name;
    o << "]@v" << a._versions.size() - 1;
    return o;
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

 protected:
  /// Add a version to the sequence of versions for this artifact
  void appendVersion(const shared_ptr<Version>& v);

 protected:
  /// The environment this artifact is managed by
  Env& _env;

 private:
  /// The name of this artifact used for pretty-printing
  string _name;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;

  /// The latest metadata version
  shared_ptr<MetadataVersion> _metadata_version;

  /// Is the latest metadata version committed to the filesystem?
  bool _metadata_committed;

  /// Metadata was last modified by this command
  weak_ptr<Command> _metadata_creator;

  /// Metadata was last modified using this reference
  weak_ptr<Reference> _metadata_ref;

  /// Has the metadata for this artifact been accessed?
  bool _metadata_accessed;
};
