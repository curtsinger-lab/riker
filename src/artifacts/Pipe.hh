#pragma once

#include "artifacts/File.hh"

class PipeArtifact final : public FileArtifact {
 public:
  using FileArtifact::FileArtifact;

  virtual string getTypeName() const noexcept final { return "Pipe"; }
};