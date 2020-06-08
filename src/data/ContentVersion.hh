#pragma once

#include <memory>
#include <optional>
#include <string>

#include "data/Fingerprint.hh"
#include "data/Version.hh"
#include "data/serializer.hh"

using std::optional;
using std::shared_ptr;
using std::string;

class Reference;

class ContentVersion final : public Version {
 public:
  /// Create a ContentVersion with no existing fingerprint
  ContentVersion() = default;

  /// Create a ContentVersion with an existing fingerprint
  ContentVersion(Fingerprint&& f) : _fingerprint(f) {}

  /// Get the name for this type of version
  virtual string getTypeName() const override { return "content"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const override;

  /// Save this version so it can be committed later
  virtual void save(const shared_ptr<Reference>& ref) override {}

  /// Commit this version to the filesystem
  virtual void commit(const shared_ptr<Reference>& ref) const override;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const override { return _fingerprint.has_value(); }

  /// Save a fingerprint of this version
  virtual void fingerprint(const shared_ptr<Reference>& ref) override;

  /// Compare this version to another version
  virtual bool matches(const shared_ptr<Version>& other) const override;

 private:
  optional<Fingerprint> _fingerprint;

  SERIALIZE(BASE(Version), _fingerprint);
};
