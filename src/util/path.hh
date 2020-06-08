#pragma once

#include <filesystem>
#include <string>

#include <limits.h>
#include <unistd.h>

using std::string;

namespace fs = std::filesystem;

inline fs::path readlink(string path) {
  char* buffer = nullptr;
  ssize_t capacity = 0;
  ssize_t bytes_read = 0;

  do {
    capacity += PATH_MAX;
    buffer = (char*)realloc(buffer, capacity);
    bytes_read = readlink(path.c_str(), buffer, capacity);
  } while (bytes_read == capacity);

  string result(buffer, buffer + bytes_read);
  free(buffer);
  return result;
}
