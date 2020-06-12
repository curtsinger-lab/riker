#pragma once

#include <memory>
#include <string>
#include <tuple>

#include "artifacts/Artifact.hh"

using std::shared_ptr;
using std::string;
using std::tuple;

class Command;
class Reference;
class Version;
class ContentVersion;

class DirArtifact final : public Artifact {
 public:
  DirArtifact(Env& env, bool committed,
              shared_ptr<MetadataVersion> mv = make_shared<MetadataVersion>()) noexcept :
      Artifact(env, committed, mv) {}

  virtual string getTypeName() const noexcept final { return "Dir"; }

  virtual tuple<shared_ptr<Artifact>, int> resolvePath(shared_ptr<Command> c,
                                                       shared_ptr<Access> ref) noexcept final;
};