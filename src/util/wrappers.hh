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
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

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

  std::string result(buffer, buffer + bytes_read);
  free(buffer);
  return result;
}

inline std::tuple<gid_t, uid_t> get_identity() noexcept {
  static std::optional<std::tuple<gid_t, uid_t>> _identity;

  if (!_identity.has_value()) {
    _identity = {getegid(), geteuid()};
  }

  return _identity.value();
}

inline const std::set<gid_t>& getgroups() noexcept {
  static std::optional<std::set<gid_t>> _groups;

  if (!_groups.has_value()) {
    _groups = std::set<gid_t>();
    int n = getgroups(0, NULL);
    gid_t sgrps[n];
    getgroups(n, sgrps);
    for (int i = 0; i < n; i++) {
      _groups.value().insert(sgrps[i]);
    }
  }

  return _groups.value();
}

extern char** environ;
inline std::unordered_map<std::string, std::string> getDefaultEnv() noexcept {
  static std::unordered_map<std::string, std::string> default_envar;

  if (default_envar.empty()) {
    for (int i = 0; environ[i] != nullptr; i++) {
      std::string variable = std::string(environ[i]);
      std::string key = variable.substr(0, variable.find("="));
      variable.erase(0, variable.find("=") + 1);
      default_envar.insert({key, variable});
    }
  }
  
  return default_envar;
}

inline std::string getSignalName(int sig) {
  static std::map<int, std::string> signals{
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

inline std::string getErrorName(int err) noexcept {
  // Set up a map from return codes to names
  static std::map<int8_t, std::string> errors = {{0, "SUCCESS"},
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

/// Check if a file exists. Fill in the stat buffer provided as a parameter
inline bool fileExists(fs::path p, struct stat& statbuf) noexcept {
  return ::lstat(p.c_str(), &statbuf) == 0;
}

/// Check whether a file exists
inline bool fileExists(fs::path p) noexcept {
  struct stat statbuf;
  return fileExists(p, statbuf);
}

/// Obtain the length of a file, in bytes.  If the file cannot be stat'ed (e.g., it doesn't exist),
/// -1 is returned.
inline loff_t fileLength(fs::path p) noexcept {
  struct stat statbuf;
  if (::lstat(p.c_str(), &statbuf) == -1) {
    return -1;
  }
  return statbuf.st_size;
}