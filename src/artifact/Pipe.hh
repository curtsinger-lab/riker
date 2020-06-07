#pragma once

#include "artifact/File.hh"

class PipeArtifact : public FileArtifact {
 public:
  using FileArtifact::FileArtifact;

  virtual string getTypeName() const override { return "Pipe"; }
};