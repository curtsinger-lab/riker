#pragma once

#include <string>

#include <linux/limits.h>
#include <unistd.h>

using std::string;

static string readlink(string path) {
  char* buffer = nullptr;
  ssize_t capacity = 0;
  ssize_t bytes_read = 0;

  do {
    capacity += PATH_MAX;
    buffer = (char*)realloc(buffer, capacity);
    bytes_read = readlink(path.c_str(), buffer, capacity);
  } while (bytes_read == capacity);

  string result(buffer);
  free(buffer);
  return result;
}
