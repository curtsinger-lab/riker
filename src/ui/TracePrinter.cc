#include "TracePrinter.hh"

#include <iostream>
#include <memory>

#include "core/Command.hh"
#include "core/RefResult.hh"
#include "core/SpecialRefs.hh"
#include "versions/Version.hh"

using std::endl;
using std::shared_ptr;

void TracePrinter::specialRef(shared_ptr<Command> c,
                              SpecialRef entity,
                              shared_ptr<RefResult> output) noexcept {
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

void TracePrinter::pipeRef(shared_ptr<Command> c,
                           shared_ptr<RefResult> read_end,
                           shared_ptr<RefResult> write_end) noexcept {
  if (c) _out << c << ": ";
  _out << "[" << read_end << ", " << write_end << "] = PipeRef()" << endl;
}

void TracePrinter::fileRef(shared_ptr<Command> c,
                           mode_t mode,
                           shared_ptr<RefResult> output) noexcept {
  if (c) _out << c << ": ";
  _out << output << " = FileRef(" << std::oct << mode << ")" << endl;
}

void TracePrinter::symlinkRef(shared_ptr<Command> c,
                              fs::path target,
                              shared_ptr<RefResult> output) noexcept {
  if (c) _out << c << ": ";
  _out << output << " = SymlinkRef(" << target << ")" << endl;
}

void TracePrinter::dirRef(shared_ptr<Command> c,
                          mode_t mode,
                          shared_ptr<RefResult> output) noexcept {
  if (c) _out << c << ": ";
  _out << output << " = DirRef(" << std::oct << mode << ")" << endl;
}

void TracePrinter::pathRef(shared_ptr<Command> c,
                           shared_ptr<RefResult> base,
                           fs::path path,
                           AccessFlags flags,
                           shared_ptr<RefResult> output) noexcept {
  if (c) _out << c << ": ";
  _out << output << " = PathRef(" << base << ", " << path << ", " << flags << ")" << endl;
}

void TracePrinter::expectResult(shared_ptr<Command> c,
                                shared_ptr<RefResult> ref,
                                int expected) noexcept {
  if (c) _out << c << ": ";
  _out << "ExpectResult(" << ref << ", " << expected << ")" << endl;
}

void TracePrinter::matchMetadata(shared_ptr<Command> c,
                                 shared_ptr<RefResult> ref,
                                 shared_ptr<MetadataVersion> expected) noexcept {
  if (c) _out << c << ": ";
  _out << "MatchMetadata(" << ref << ", " << expected << ")" << endl;
}

void TracePrinter::matchContent(shared_ptr<Command> c,
                                shared_ptr<RefResult> ref,
                                shared_ptr<Version> expected) noexcept {
  if (c) _out << c << ": ";
  _out << "MatchContent(" << ref << ", " << expected << ")" << endl;
}

void TracePrinter::updateMetadata(shared_ptr<Command> c,
                                  shared_ptr<RefResult> ref,
                                  shared_ptr<MetadataVersion> written) noexcept {
  if (c) _out << c << ": ";
  _out << "UpdateMetadata(" << ref << ", " << written << ")" << endl;
}

void TracePrinter::updateContent(shared_ptr<Command> c,
                                 shared_ptr<RefResult> ref,
                                 shared_ptr<Version> written) noexcept {
  if (c) _out << c << ": ";
  _out << "UpdateContent(" << ref << ", " << written << ")" << endl;
}

void TracePrinter::launch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept {
  if (c) _out << c << ": ";
  _out << "Launch(" << child << ")" << endl;
}

void TracePrinter::join(shared_ptr<Command> c,
                        shared_ptr<Command> child,
                        int exit_status) noexcept {
  if (c) _out << c << ": ";
  _out << "Join(" << child << ", " << exit_status << ")" << endl;
}

void TracePrinter::exit(shared_ptr<Command> c, int exit_status) noexcept {
  if (c) _out << c << ": ";
  _out << "Exit(" << exit_status << ")" << endl;
}