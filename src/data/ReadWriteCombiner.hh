#include <memory>

#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "versions/ContentVersion.hh"

class MetadataVersion;

template <class Next>
class MetadataReadCombiner;

template <class Next>
class MetadataWriteCombiner;

template <class Next>
class ContentReadCombiner;

template <class Next>
class ContentWriteCombiner;

/**
 * This class receives and filters a sequence of IR steps to combine repeated writes through the
 * same reference. It is implemented with two separate write combiners: one for metadata and one for
 * content.
 *
 * The WriteCombiner class expects a template parameter that is an IRSink, which will receive all of
 * the original trace steps, except for the steps filtered out.
 */
template <class Next>
class ReadWriteCombiner
    : public MetadataWriteCombiner<
          ContentWriteCombiner<MetadataReadCombiner<ContentReadCombiner<Next>>>> {};

/// This class implements read-combining logic for metadata accesses.
template <class Next>
class MetadataReadCombiner : public Next {
 public:
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion expected) noexcept override {
    // Post-build checks should be emitted as-is
    if (scenario & Scenario::PostBuild) {
      Next::matchMetadata(command, scenario, ref, expected);
      return;
    }

    // Is this this a read from the same command using the same reference as the last one?
    if (_last_reader == command && _last_ref == ref) {
      // Yes. The read is unnecessary.
      return;
    }

    // Record the last read
    _last_reader = command;
    _last_ref = ref;

    // Pass the read along
    Next::matchMetadata(command, scenario, ref, expected);
  }

  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion writing) noexcept override {
    // Clear the last read
    _last_reader.reset();
    _last_ref = -1;

    // Pass the write along
    Next::updateMetadata(command, ref, writing);
  }

 private:
  /// The last command to read an artifact's metadata
  std::shared_ptr<Command> _last_reader;

  /// The last reference used to read an artifact's metadata
  Ref::ID _last_ref;
};

/// This class implements write-combining logic for metadata accesses.
template <class Next>
class MetadataWriteCombiner : public Next {
 public:
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion expected) noexcept override {
    // Does this read match the last write?
    if (command == _last_writer && ref == _last_ref) {
      // Yes. We can skip the read, since it's just reading the last write.
      // We do not need to mark the last write as accessed because it is read by the writing command

    } else {
      // No. If the last write hasn't been emitted, emit it now
      if (!_emitted) {
        Next::updateMetadata(_last_writer, _last_ref, _last_written.value());
        _emitted = true;
      }

      // The last write may have been accessed, so it should not be coalesced
      _accessed = true;

      // Emit the matchContent predicate that performs the read
      Next::matchMetadata(command, scenario, ref, expected);
    }
  }

  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion writing) noexcept override {
    // We can coalesce this new write with the previous write if the command and reference are the
    // same and the last write has not been accessed
    if (command == _last_writer && ref == _last_ref && !_accessed) {
      // Yes. We can skip the write entirely. The write is not emitted.
      _last_written = writing;
      _emitted = false;

    } else {
      // No. Emit the previous write if it hasn't been passed along already
      if (!_emitted) {
        Next::updateMetadata(_last_writer, _last_ref, _last_written.value());
        _emitted = true;
      }

      // Save this write and do not emit it yet
      _last_writer = command;
      _last_ref = ref;
      _last_written = writing;
      _emitted = false;
      _accessed = false;
    }
  }

  /**
   * When a command exits we have to check to see if there is a deferred write from that command. If
   * so, emit the deferred write before the exit step.
   */
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override {
    // If the last write hasn't been emitted and the exiting command performed that write, emit it
    if (!_emitted && _last_writer == command) {
      Next::updateMetadata(_last_writer, _last_ref, _last_written.value());
      _emitted = true;
    }

    Next::exit(command, exit_status);
  }

 private:
  /// The last command to write an artifact's metadata
  std::shared_ptr<Command> _last_writer;

  /// The last reference used to write an artifact's metadata
  Ref::ID _last_ref;

  /// The last version written
  std::optional<MetadataVersion> _last_written;

  /// Has the last write been emitted?
  bool _emitted = true;

  /// Has the last write been accessed by a command other than the writer?
  bool _accessed = false;
};

/// This class implements read-combining logic for content accesses.
template <class Next>
class ContentReadCombiner : public Next {
 public:
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override {
    if (scenario & Scenario::PostBuild) {
      Next::matchContent(command, scenario, ref, expected);
      return;
    }

    // Is this this a read from the same command using the same reference as the last one?
    if (_last_reader == command && _last_ref == ref) {
      // Yes. The read is unnecessary.
      return;
    }

    // Record the last read
    _last_reader = command;
    _last_ref = ref;

    // Pass the read along
    Next::matchContent(command, scenario, ref, expected);
  }

  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> writing) noexcept override {
    // Clear the last read
    _last_reader.reset();
    _last_ref = -1;

    // Pass the write along
    Next::updateContent(command, ref, writing);
  }

 private:
  /// The last command to read an artifact's metadata
  std::shared_ptr<Command> _last_reader;

  /// The last reference used to read an artifact's metadata
  Ref::ID _last_ref;
};

/// This class implements write-combining logic for content accesses.
template <class Next>
class ContentWriteCombiner : public Next {
 public:
  /**
   * When a trace accesses content through a reference, we can elide that access if it is just
   * accessing the last write. If it's a different access we have to emit the previously-deferred
   * write, then pass along the content access.
   */
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override {
    // Does this read match the last write?
    if (command == _last_writer && ref == _last_ref) {
      // Yes. We can skip the read, since it's just reading the last write.
      // We do not need to mark the last write as accessed because it is read by the writing command

    } else {
      // No. If the last write hasn't been emitted, emit it now
      if (!_emitted) {
        Next::updateContent(_last_writer, _last_ref, _last_written);
        _emitted = true;
      }

      // The last write may have been accessed, so it should not be coalesced
      _accessed = true;

      // Emit the matchContent predicate that performs the read
      Next::matchContent(command, scenario, ref, expected);
    }
  }

  /**
   * When a trace writes content through a reference, we do _not_ pass that write along immediately.
   * Instead, on future writes we can decide if the previous write can be coalesced with the current
   * one. This is only possible for writes by the same command using the same reference. If a write
   * cannot be coalesced, we emit a previously-deferred write and then defer the new one.
   */
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> writing) noexcept override {
    // We can coalesce this new write with the previous write if the command and reference are the
    // same, the last write has not been accessed, and the specific versions allow coalescing
    if (command == _last_writer && ref == _last_ref && !_accessed &&
        _last_written->canCoalesceWith(writing)) {
      // Yes. We can skip the write entirely. The write is not emitted.
      _last_written = writing;
      _emitted = false;

    } else {
      // No. Emit the previous write if it hasn't been passed along already
      if (!_emitted) {
        Next::updateContent(_last_writer, _last_ref, _last_written);
        _emitted = true;
      }

      // Save this write and do not emit it yet
      _last_writer = command;
      _last_ref = ref;
      _last_written = writing;
      _emitted = false;
      _accessed = false;
    }
  }

  /**
   * When a command exits we have to check to see if there is a deferred write from that command. If
   * so, emit the deferred write before the exit step.
   */
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override {
    // If the last write hasn't been emitted and the exiting command performed that write, emit it
    if (!_emitted && _last_writer == command) {
      Next::updateContent(_last_writer, _last_ref, _last_written);
      _emitted = true;
    }

    Next::exit(command, exit_status);
  }

 private:
  /// The last command to write an artifact's content
  std::shared_ptr<Command> _last_writer;

  /// The last reference used to write an artifact's content
  Ref::ID _last_ref;

  /// The last version written
  std::shared_ptr<ContentVersion> _last_written;

  /// Has the last write been emitted?
  bool _emitted = true;

  /// Has the last write been accessed by a command other than the writer?
  bool _accessed = false;
};
