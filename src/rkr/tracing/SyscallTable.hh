#pragma once

#include <array>
#include <cstddef>

#include <sys/user.h>
#include <syscall.h>

#include "tracing/inject.h"

class Thread;

/// The type of a system call handler
typedef void (*handler_t)(Thread& __thr, const user_regs_struct& __regs, ssize_t channel);

/// The default handler
constexpr handler_t default_handler = [](Thread& t, const user_regs_struct& regs, ssize_t channel) {
};

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
  void runHandler(Thread& t, const user_regs_struct& regs, ssize_t channel = -1) const {
    _handler(t, regs, channel);
  }

 private:
  const char* _name;
  bool _traced;
  handler_t _handler;
};

/// The maximum number of system calls
#define SYSCALL_COUNT 512

/**
 * Create a system call table that specifies names and handlers for traced system calls
 */
class SyscallTable {
 private:
  constexpr SyscallTable();
  static const SyscallTable _the_table;

 public:
  static constexpr const SyscallEntry& get(int i) { return _the_table._syscalls[i]; }

  static constexpr size_t size() { return _the_table._syscalls.size(); }

 private:
  std::array<SyscallEntry, SYSCALL_COUNT> _syscalls;
};