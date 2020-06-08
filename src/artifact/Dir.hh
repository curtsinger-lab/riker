#pragma once

#include <memory>
#include <string>

#include "artifact/Artifact.hh"

using std::shared_ptr;
using std::string;

class Command;
class Reference;
class Version;
class ContentVersion;

class DirArtifact final : public Artifact {
 public:
  DirArtifact(Env& env, bool committed,
              const shared_ptr<MetadataVersion> mv = make_shared<MetadataVersion>()) :
      Artifact(env, committed, mv) {}

  virtual string getTypeName() const override { return "Dir"; }
};