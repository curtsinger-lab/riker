#pragma once

#include <memory>
#include <optional>
#include <string>

#include "data/Metadata.hh"
#include "data/Version.hh"
#include "data/serializer.hh"

using std::optional;
using std::shared_ptr;
using std::string;

class Reference;

class MetadataVersion final : public Version {
 public:
  /// Create a new metadata version with unknown metadata
  MetadataVersion() = default;

  /// Cerate a new metadata version with existing metadata
  MetadataVersion(Metadata&& m) : _metadata(m) {}

  /// Get the name for this type of version
  virtual string getTypeName() const override { return "metadata"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const override { return _metadata.has_value(); }

  /// Save this version so it can be committed later
  virtual void save(const shared_ptr<Reference>& ref) override;

  /// Commit this version to the filesystem
  virtual void commit(const shared_ptr<Reference>& ref) const override;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const override { return _metadata.has_value(); }

  /// Save a fingerprint of this version
  virtual void fingerprint(const shared_ptr<Reference>& ref) override { save(ref); }

  /// Compare this version to another version
  virtual bool matches(const shared_ptr<Version>& other) const override;

 private:
  optional<Metadata> _metadata;

  SERIALIZE(BASE(Version), _metadata);
};