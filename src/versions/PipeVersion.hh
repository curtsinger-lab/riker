#pragma once

#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::list;
using std::shared_ptr;
using std::string;
using std::tuple;

class Build;
class Command;
class Ref;

class PipeWriteVersion : public Version {
 public:
  /// Create a new version to track a write to a pipe
  PipeWriteVersion() noexcept : Version() {}

  /// Get a short name for this version type
  virtual string getTypeName() const noexcept override { return "pipe write"; }

  /// Check if a written pipe version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  /// A pipe write cannot be committed
  virtual void commit(fs::path path) noexcept override {}

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[pipe write]"; }

 private:
  SERIALIZE(BASE(Version));
};

class PipeCloseVersion : public PipeWriteVersion {
 public:
  /// Get a short name for thsi version type
  virtual string getTypeName() const noexcept override { return "pipe close"; }

  /// Check if a this version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    return static_cast<bool>(other->as<PipeCloseVersion>());
  }

  /// Pipe closes can always be committed
  virtual bool canCommit() const noexcept override { return true; }

  /// Commit does nothing for pipe closes
  virtual void commit(fs::path path) noexcept override {}

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[pipe close]"; }

 private:
  SERIALIZE(BASE(PipeWriteVersion));
};

class PipeReadVersion : public Version {
 public:
  /// Create a new version that tracks a read from a pipe. The read observes some number of writes.
  PipeReadVersion(list<shared_ptr<PipeWriteVersion>> observed) noexcept : _observed(observed) {}

  /// Get a short name for this version type
  virtual string getTypeName() const noexcept override { return "pipe read"; }

  /// Check if a read pipe version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  /// A pipe read cannot be committed
  virtual void commit(fs::path path) noexcept override {}

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[pipe read " << _observed.size() << "]";
  }

 private:
  /// The list of writes this read version observes
  list<shared_ptr<PipeWriteVersion>> _observed;

  // Create default constructor and declare fields for serialization
  PipeReadVersion() noexcept = default;
  SERIALIZE(BASE(Version), _observed);
};