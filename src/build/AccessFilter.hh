#pragma once

#include <memory>
#include <set>
#include <utility>

#include "core/Command.hh"
#include "core/IR.hh"
#include "versions/Version.hh"

using std::pair;
using std::set;
using std::shared_ptr;

/**
 * This class is used to determine whether a read/write operation must be added to the trace, or if
 * it can safely be elided. Each type of access (metdata, contents, etc.) can use a separate filter
 * because two accesses cannot alias each other if they refer to different version types.
 *
 * The filter implements the following logic:
 * - Repeated reads by the same command through the same reference can be skipped
 * - Repeated writes by the same command through the same reference can be skipped, unless the
 *   previously-written version has been accessed
 * - Any new write serves as a barrier, so all reads after that write are considered new, except for
 *   reads by the writing command through the same reference
 *
 * This may seem overly-conservative, but it is necessary because any two references could alias on
 * some future rebuild. Therefore, any write to metadata could change the outcome of any future
 * read, so those reads after each write must be checked (and therefore must appear in the trace).
 */
class AccessFilter {
 public:
  // Record the effect of a read by command c using reference ref
  void read(Command* c, shared_ptr<Reference> ref) noexcept {
    // Command c can read through reference ref without logging until the next write
    _observed.emplace(c, ref.get());
  }

  // Record the effect of a write by command c using reference ref
  void write(Command* c, shared_ptr<Reference> ref, shared_ptr<Version> written) noexcept {
    // All future reads could be affected by this write, so they need to be logged
    _observed.clear();

    // Keep track of the last write
    _last_writer = c;
    _last_write_ref = ref.get();
    _last_written_version = written.get();

    // The writer can observe its own written value without logging
    _observed.emplace(c, ref.get());
  }

  // Does command c need to add a read through reference ref to its trace?
  bool readRequired(Command* c, shared_ptr<Reference> ref) noexcept {
    // If this optimization is disabled, the read is always required
    if (!options::combine_reads) return true;

    // If this command has already read through this reference, the read is not required
    if (_observed.find({c, ref.get()}) != _observed.end()) return false;

    // Otherwise the read is required
    return true;
  }

  // Does command c need to add a write through reference ref to its trace?
  bool writeRequired(Command* c, shared_ptr<Reference> ref) noexcept {
    // If this optimization is disabled, the write is always required
    if (!options::combine_writes) return true;

    // If this is the first write through the filter, it must be added to the trace
    if (!_last_written_version) return true;

    // If the last version written through this filter was accessed, add a new write to the trace
    if (_last_written_version->isAccessed()) return true;

    // If a different command is writing, add a new write to the trace
    if (c != _last_writer) return true;

    // If the same command is using a different reference to write, add a new write to the trace
    if (ref.get() != _last_write_ref) return true;

    // This write is by the same command as the last write using the same reference.
    // The previously-written value has not been accessed, so we do not need to log a new write.
    return false;
  }

 private:
  Command* _last_writer = nullptr;
  Reference* _last_write_ref = nullptr;
  Version* _last_written_version = nullptr;
  set<pair<Command*, Reference*>> _observed;
};
