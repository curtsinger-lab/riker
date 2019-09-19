#pragma once

#include "core/File.hh"

struct FileDescriptor {
  size_t location_index;  // Used in Process::fds
  File* file;             // Used in Command::initial_fds
  int access_mode;
  bool cloexec;

  FileDescriptor() {}

  FileDescriptor(size_t location_index, int access_mode, bool cloexec) :
      location_index(location_index),
      access_mode(access_mode),
      cloexec(cloexec) {}
};
