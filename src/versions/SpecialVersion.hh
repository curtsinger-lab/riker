#pragma once

#include <string>

#include "util/serializer.hh"
#include "versions/ContentVersion.hh"

class SpecialVersion final : public ContentVersion {
 public:
  /// Create a SpecialVersion
  SpecialVersion(bool can_commit) noexcept : _can_commit(can_commit) {}

  /// Can this version be committed?
  virtual bool canCommit() const noexcept override { return _can_commit; }

  /// Get the name for this type of version
  virtual std::string getTypeName() const noexcept override { return "special"; }

  /// Pretty printer
  virtual std::ostream& print(std::ostream& o) const noexcept override { return o << "[special]"; }

 private:
  bool _can_commit;

  SpecialVersion() noexcept = default;
  SERIALIZE(BASE(ContentVersion), _can_commit);
};
