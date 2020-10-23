#pragma once

#include <filesystem>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <tuple>

#include <grp.h>
#include <limits.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

using std::map;
using std::optional;
using std::set;
using std::string;
using std::tuple;

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

inline string getSignalName(int sig) {
  static map<int, string> signals{
      {SIGHUP, "SIGHUP"},       {SIGINT, "SIGINT"},       {SIGQUIT, "SIGQUIT"},
      {SIGILL, "SIGILL"},       {SIGTRAP, "SIGTRAP"},     {SIGABRT, "SIGABRT"},
      {SIGIOT, "SIGIOT"},       {SIGBUS, "SIGBUS"},       {SIGFPE, "SIGFPE"},
      {SIGKILL, "SIGKILL"},     {SIGUSR1, "SIGUSR1"},     {SIGSEGV, "SIGSEGV"},
      {SIGUSR2, "SIGUSR2"},     {SIGPIPE, "SIGPIPE"},     {SIGALRM, "SIGALRM"},
      {SIGTERM, "SIGTERM"},     {SIGSTKFLT, "SIGSTKFLT"}, {SIGCHLD, "SIGCHLD"},
      {SIGCLD, "SIGCLD"},       {SIGCONT, "SIGCONT"},     {SIGSTOP, "SIGSTOP"},
      {SIGTSTP, "SIGTSTP"},     {SIGTTIN, "SIGTTIN"},     {SIGTTOU, "SIGTTOU"},
      {SIGURG, "SIGURG"},       {SIGXCPU, "SIGXCPU"},     {SIGXFSZ, "SIGXFSZ"},
      {SIGVTALRM, "SIGVTALRM"}, {SIGPROF, "SIGPROF"},     {SIGWINCH, "SIGWINCH"},
      {SIGIO, "SIGIO"},         {SIGPOLL, "SIGPOLL"},     {SIGPWR, "SIGPWR"},
      {SIGSYS, "SIGSYS"}};

  auto iter = signals.find(sig);
  if (iter == signals.end()) {
    return "UNKNOWN (" + std::to_string(sig) + ")";
  } else {
    return iter->second;
  }
}

inline string getErrorName(int err) noexcept {
  // Set up a map from return codes to names
  static map<int8_t, string> errors = {{0, "SUCCESS"},
                                       {EACCES, "EACCES"},
                                       {EDQUOT, "EDQUOT"},
                                       {EEXIST, "EEXIST"},
                                       {EINVAL, "EINVAL"},
                                       {EISDIR, "EISDIR"},
                                       {ELOOP, "ELOOP"},
                                       {ENOENT, "ENOENT"},
                                       {ENOEXEC, "ENOEXEC"},
                                       {ENOTDIR, "ENOTDIR"},
                                       {ENAMETOOLONG, "ENAMETOOLONG"}};

  auto iter = errors.find(err);
  if (iter == errors.end()) {
    return "UNKNOWN (" + std::to_string(err) + ")";
  } else {
    return iter->second;
  }
}
