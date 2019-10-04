#pragma once

#include <cstddef>

class File;

using std::shared_ptr;

struct FileDescriptor {
  shared_ptr<File> file;
  int access_mode;
  bool cloexec;
  
  FileDescriptor() {}

  FileDescriptor(shared_ptr<File> file, int access_mode, bool cloexec) :
      file(file),
      access_mode(access_mode),
      cloexec(cloexec) {}
};
