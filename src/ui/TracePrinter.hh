#pragma once

#include <memory>
#include <ostream>

#include "build/Resolution.hh"
#include "core/SpecialRefs.hh"
#include "core/TraceHandler.hh"

using std::endl;
using std::ostream;
using std::shared_ptr;

class Command;
class RefResult;

template <class T>
class TracePrinter : public TraceHandler {
 public:
  /// Create a trace printer that writes to a provided ostream
  TracePrinter(T& out) : _out(out) {}

  /// Create a trace printer that writes to a provided ostream (rvalue reference form)
  TracePrinter(T&& out) : _out(out) {}

  virtual void specialRef(shared_ptr<Command> c,
                          SpecialRef entity,
                          shared_ptr<RefResult> output) noexcept override {
    if (c) _out << c << ": ";
    _out << output << " = ";
    switch (entity) {
      case SpecialRef::stdin:
        _out << "STDIN";
        break;

      case SpecialRef::stdout:
        _out << "STDOUT";
        break;

      case SpecialRef::stderr:
        _out << "STDERR";
        break;

      case SpecialRef::root:
        _out << "ROOT";
        break;

      case SpecialRef::cwd:
        _out << "CWD";
        break;

      case SpecialRef::launch_exe:
        _out << "LAUNCH_EXE";
        break;
    }
    _out << endl;
  }

  virtual void pipeRef(shared_ptr<Command> c,
                       shared_ptr<RefResult> read_end,
                       shared_ptr<RefResult> write_end) noexcept override {
    if (c) _out << c << ": ";
    _out << "[" << read_end << ", " << write_end << "] = PipeRef()" << endl;
  }

  virtual void fileRef(shared_ptr<Command> c,
                       mode_t mode,
                       shared_ptr<RefResult> output) noexcept override {
    if (c) _out << c << ": ";
    _out << output << " = FileRef(" << std::oct << mode << ")" << endl;
  }

  virtual void symlinkRef(shared_ptr<Command> c,
                          fs::path target,
                          shared_ptr<RefResult> output) noexcept override {
    if (c) _out << c << ": ";
    _out << output << " = SymlinkRef(" << target << ")" << endl;
  }

  virtual void dirRef(shared_ptr<Command> c,
                      mode_t mode,
                      shared_ptr<RefResult> output) noexcept override {
    if (c) _out << c << ": ";
    _out << output << " = DirRef(" << std::oct << mode << ")" << endl;
  }

  virtual void pathRef(shared_ptr<Command> c,
                       shared_ptr<RefResult> base,
                       fs::path path,
                       AccessFlags flags,
                       shared_ptr<RefResult> output) noexcept override {
    if (c) _out << c << ": ";
    _out << output << " = PathRef(" << base << ", " << path << ", " << flags << ")" << endl;
  }

  virtual void expectResult(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            int expected) noexcept override {
    if (c) _out << c << ": ";
    _out << "ExpectResult(" << ref << ", " << errors[expected] << ")" << endl;
  }

  virtual void matchMetadata(shared_ptr<Command> c,
                             shared_ptr<RefResult> ref,
                             shared_ptr<MetadataVersion> expected) noexcept override {
    if (c) _out << c << ": ";
    _out << "MatchMetadata(" << ref << ", " << expected << ")" << endl;
  }

  virtual void matchContent(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> expected) noexcept override {
    if (c) _out << c << ": ";
    _out << "MatchContent(" << ref << ", " << expected << ")" << endl;
  }

  virtual void updateMetadata(shared_ptr<Command> c,
                              shared_ptr<RefResult> ref,
                              shared_ptr<MetadataVersion> written) noexcept override {
    if (c) _out << c << ": ";
    _out << "UpdateMetadata(" << ref << ", " << written << ")" << endl;
  }

  virtual void updateContent(shared_ptr<Command> c,
                             shared_ptr<RefResult> ref,
                             shared_ptr<Version> written) noexcept override {
    if (c) _out << c << ": ";
    _out << "UpdateContent(" << ref << ", " << written << ")" << endl;
  }

  virtual void launch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept override {
    if (c) _out << c << ": ";
    _out << "Launch(" << child << ")" << endl;
  }

  virtual void join(shared_ptr<Command> c,
                    shared_ptr<Command> child,
                    int exit_status) noexcept override {
    if (c) _out << c << ": ";
    _out << "Join(" << child << ", " << exit_status << ")" << endl;
  }

  virtual void exit(shared_ptr<Command> c, int exit_status) noexcept override {
    if (c) _out << c << ": ";
    _out << "Exit(" << exit_status << ")" << endl;
  }

 private:
  T& _out;
};
