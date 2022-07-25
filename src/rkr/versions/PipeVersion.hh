#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <ostream>
#include <string>

#include "versions/ContentVersion.hh"

namespace fs = std::filesystem;

class PipeWriteVersion : public ContentVersion {
 public:
  /// Create a new version to track a write to a pipe
  PipeWriteVersion() noexcept : ContentVersion() {}

  /// Get a short name for this version type
  virtual std::string getTypeName() const noexcept override { return "pipe write"; }

  /// Check if a written pipe version matches another
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override;

  /// Print this version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    return o << "[pipe write]";
  }
};

template <>
struct fmt::formatter<PipeWriteVersion> : fmt::ostream_formatter {};

class PipeCloseVersion : public PipeWriteVersion {
 public:
  /// Get a short name for thsi version type
  virtual std::string getTypeName() const noexcept override { return "pipe close"; }

  /// Check if a this version matches another
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override {
    return static_cast<bool>(other->as<PipeCloseVersion>());
  }

  /// Print this version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    return o << "[pipe close]";
  }
};

template <>
struct fmt::formatter<PipeCloseVersion> : fmt::ostream_formatter {};

class PipeReadVersion : public ContentVersion {
 public:
  /// Create a new version that tracks a read from a pipe.
  PipeReadVersion() noexcept = default;

  /// Get a short name for this version type
  virtual std::string getTypeName() const noexcept override { return "pipe read"; }

  /// Check if a read pipe version matches another
  virtual bool matches(std::shared_ptr<ContentVersion> other) noexcept override;

  /// Print this version
  virtual std::ostream& print(std::ostream& o) const noexcept override {
    return o << "[pipe read]";
  }
};

template <>
struct fmt::formatter<PipeReadVersion> : fmt::ostream_formatter {};
