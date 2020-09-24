#pragma once

#include <filesystem>
#include <optional>
#include <set>
#include <string>

#include <grp.h>
#include <limits.h>
#include <sys/types.h>
#include <unistd.h>

using std::optional;
using std::set;
using std::string;

namespace fs = std::filesystem;

inline fs::path readlink(fs::path path) noexcept {
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

inline tuple<gid_t, uid_t> get_identity() noexcept {
  static optional<tuple<gid_t, uid_t>> _identity;

  if (!_identity.has_value()) {
    _identity = {getegid(), geteuid()};
  }

  return _identity.value();
}

inline const set<gid_t>& getgroups() noexcept {
  static optional<set<gid_t>> _groups;

  if (!_groups.has_value()) {
    _groups = set<gid_t>();
    int n = getgroups(0, NULL);
    gid_t sgrps[n];
    getgroups(n, sgrps);
    for (int i = 0; i < n; i++) {
      _groups.value().insert(sgrps[i]);
    }
  }

  return _groups.value();
}
