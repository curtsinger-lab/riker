#include <memory>

#include "artifacts/Artifact.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "versions/ContentVersion.hh"

class MetadataVersion;

/**
 * This class receives and filters a sequence of IR steps to combine repeated writes through the
 * same reference.
 *
 * The WriteCombiner class expects a template parameter that is an IRSink, which will receive all
 * of the original trace steps, except for the steps filtered out.
 */
template <class Next>
class WriteCombiner : public Next {
 public:
  /// Handle a MatchContent IR step
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override {
    // Does this read match the last write?
    if (command == _last_writer && ref == _last_ref) {
      // Yes. We can skip the read, since it's just reading the last write
    } else {
      // No. If the last write hasn't been emitted, emit it now
      if (!_emitted) {
        Next::updateContent(_last_writer, _last_ref, _last_written);
      }

      _last_writer.reset();
      _last_ref = -1;
      _last_written.reset();
      _emitted = true;

      Next::matchContent(command, scenario, ref, expected);
    }
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> writing) noexcept override {
    // Does this write match the last one, and does the writing version allow coalescing writes?
    if (command == _last_writer && ref == _last_ref && _last_written->canCoalesceWith(writing)) {
      // Yes. We can skip the write entirely. The write is not emitted.
      _last_written = writing;
      _emitted = false;

    } else {
      // No. Emit the previous write if it hasn't been passed along already
      if (!_emitted) {
        Next::updateContent(_last_writer, _last_ref, _last_written);
        _emitted = true;
      }

      _last_writer = command;
      _last_ref = ref;
      _last_written = writing;
      _emitted = false;
      // Next::updateContent(command, ref, writing);
    }
  }

  /// Handle an Exit IR step
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
};
