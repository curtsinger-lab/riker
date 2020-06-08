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

class FileArtifact : public Artifact {
 public:
  FileArtifact(Env& env, bool committed,
               shared_ptr<MetadataVersion> mv = make_shared<MetadataVersion>(),
               shared_ptr<ContentVersion> cv = make_shared<ContentVersion>());

  virtual void checkFinalState(shared_ptr<Reference> ref) override;

  virtual shared_ptr<Command> getContentCreator() const override {
    return _content_filter.getLastWriter();
  }

  virtual void saveFingerprint(shared_ptr<Reference> ref) override;

  virtual shared_ptr<ContentVersion> accessContents(shared_ptr<Command> c,
                                                    shared_ptr<Reference> ref) override;

  virtual shared_ptr<ContentVersion> setContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                                 shared_ptr<ContentVersion> v = nullptr) override;

  virtual string getTypeName() const override { return "File"; }

 private:
  /// The latest content version
  shared_ptr<ContentVersion> _content_version;

  /// The access filter that controls content interactions
  AccessFilter _content_filter;
};