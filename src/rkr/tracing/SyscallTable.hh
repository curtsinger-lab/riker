#pragma once

#include <array>
#include <cstddef>
#include <filesystem>
#include <string>
#include <vector>

#include <sys/user.h>
#include <syscall.h>

#include "tracing/Flags.hh"
#include "tracing/Tracer.hh"
#include "tracing/inject.h"
#include "util/stats.hh"

namespace fs = std::filesystem;

class Build;
class Thread;

/// The type of a system call handler
typedef void (*handler_t)(Build& b, Thread& t, const user_regs_struct& regs);

/// The default handler
constexpr handler_t default_handler = [](Build& b, Thread& t, const user_regs_struct& regs) {};

/**
 * A system call entry records the name of a system call, whether or not it should be traced, and
 * the handler that should run if it is traced.
 */
class SyscallEntry {
 public:
  /// Default constructor to fill in the syscall table
  constexpr SyscallEntry() : _name("unknown"), _traced(false), _handler(default_handler) {}

  /// Create a named but untraced entry
  constexpr SyscallEntry(const char* name) :
      _name(name), _traced(false), _handler(default_handler) {}

  /// Create an entry with a name and handler
  constexpr SyscallEntry(const char* name, handler_t handler) :
      _name(name), _traced(true), _handler(handler) {}

  /// Get the name of this system call
  const char* getName() const { return _name; }

  /// Check if this system call should be traced
  bool isTraced() const { return _traced; }

  /// Run the handler for this system call
  void runHandler(Build& b, Thread& t, const user_regs_struct& regs) const { _handler(b, t, regs); }

 private:
  const char* _name;
  bool _traced;
  handler_t _handler;
};

/// The maximum number of system calls
#define SYSCALL_COUNT 512

/// A helper macro for use in the SyscallTable constructor
#define TRACE(name)                                                                  \
  _syscalls[__NR_##name] =                                                           \
      SyscallEntry(#name, [](Output& out, Thread& t, const user_regs_struct& regs) { \
        stats::syscalls++;                                                           \
        t.invokeHandler(&Thread::_##name, out, regs);                                \
      });

/**
 * Create a system call table that specifies names and handlers for traced system calls
 */
template <class Output>
class SyscallTable {
 private:
  constexpr SyscallTable() {
// Include architecture-specific system call table
#if defined(__x86_64__) || defined(_M_X64)
#include "amd64/syscalls.hh"
#elif defined(__aarch64__) || defined(_M_ARM64)
#include "arm64/syscalls.hh"
#else
#error "Unsupported architecture"
#endif
  }

 public:
  static constexpr const SyscallEntry& get(int i) { return _the_table._syscalls[i]; }

  static constexpr size_t size() { return _the_table._syscalls.size(); }

 private:
  std::array<SyscallEntry, SYSCALL_COUNT> _syscalls;

  inline static const SyscallTable _the_table;
};
