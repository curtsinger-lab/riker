#pragma once

// Action cases:
//  LAUNCH(c : Command, file descriptors)
//  SET_METADATA(r : Ref, v : Artifact::VersionRef)
//  SET_CONTENTS(r : Ref, v : Artifact::VersionRef)

class Action {
 public:
  class Launch;
  class SetMetadata;
  class SetContents;
};

class Action::Launch : public Action {};

class Action::SetMetadata : public Action {};

class Action::SetContents : public Action {};
