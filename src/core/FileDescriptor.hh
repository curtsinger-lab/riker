#pragma once

#include <cstddef>

class Artifact;

using std::shared_ptr;

struct FileDescriptor {
  Artifact* artifact = nullptr;
  int access_mode;
  bool cloexec;

  FileDescriptor() {}

  FileDescriptor(Artifact* artifact, int access_mode, bool cloexec) :
      artifact(artifact), access_mode(access_mode), cloexec(cloexec) {}
};
