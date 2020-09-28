#pragma once

#include <memory>
#include <ostream>

#include "interfaces/TraceHandler.hh"
#include "runtime/Resolution.hh"

using std::endl;
using std::ostream;
using std::shared_ptr;
using std::string;

class Command;
class RefResult;

class TracePrinter : public TraceHandler {
 public:
  /// Create a trace printer that writes to a provided ostream
  TracePrinter(ostream& out) : _out(out) {}

  /// Create a trace printer that writes to a provided ostream (rvalue reference form)
  TracePrinter(ostream&& out) : _out(out) {}

  virtual void specialRef(shared_ptr<Command> c,
                          ::SpecialRef entity,
                          shared_ptr<RefResult> output) noexcept override {
    _out << SpecialRefPrinter{c, entity, output} << endl;
  }

  virtual void pipeRef(shared_ptr<Command> c,
                       shared_ptr<RefResult> read_end,
                       shared_ptr<RefResult> write_end) noexcept override {
    _out << PipeRefPrinter{c, read_end, write_end} << endl;
  }

  virtual void fileRef(shared_ptr<Command> c,
                       mode_t mode,
                       shared_ptr<RefResult> output) noexcept override {
    _out << FileRefPrinter{c, mode, output} << endl;
  }

  virtual void symlinkRef(shared_ptr<Command> c,
                          fs::path target,
                          shared_ptr<RefResult> output) noexcept override {
    _out << SymlinkRefPrinter{c, target, output} << endl;
  }

  virtual void dirRef(shared_ptr<Command> c,
                      mode_t mode,
                      shared_ptr<RefResult> output) noexcept override {
    _out << DirRefPrinter{c, mode, output} << endl;
  }

  virtual void pathRef(shared_ptr<Command> c,
                       shared_ptr<RefResult> base,
                       fs::path path,
                       AccessFlags flags,
                       shared_ptr<RefResult> output) noexcept override {
    _out << PathRefPrinter{c, base, path, flags, output} << endl;
  }

  virtual void expectResult(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            int expected) noexcept override {
    _out << ExpectResultPrinter{c, ref, expected} << endl;
  }

  virtual void matchMetadata(shared_ptr<Command> c,
                             shared_ptr<RefResult> ref,
                             shared_ptr<MetadataVersion> expected) noexcept override {
    _out << MatchMetadataPrinter{c, ref, expected} << endl;
  }

  virtual void matchContent(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> expected) noexcept override {
    _out << MatchContentPrinter{c, ref, expected} << endl;
  }

  virtual void updateMetadata(shared_ptr<Command> c,
                              shared_ptr<RefResult> ref,
                              shared_ptr<MetadataVersion> written) noexcept override {
    _out << UpdateMetadataPrinter{c, ref, written} << endl;
  }

  virtual void updateContent(shared_ptr<Command> c,
                             shared_ptr<RefResult> ref,
                             shared_ptr<Version> written) noexcept override {
    _out << UpdateContentPrinter{c, ref, written} << endl;
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(shared_ptr<Command> c,
                        shared_ptr<RefResult> dir,
                        fs::path name,
                        shared_ptr<RefResult> target) noexcept override {
    _out << AddEntryPrinter{c, dir, name, target} << endl;
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(shared_ptr<Command> c,
                           shared_ptr<RefResult> dir,
                           fs::path name,
                           shared_ptr<RefResult> target) noexcept override {
    _out << RemoveEntryPrinter{c, dir, name, target} << endl;
  }

  virtual void launch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept override {
    _out << LaunchPrinter{c, child} << endl;
  }

  virtual void join(shared_ptr<Command> c,
                    shared_ptr<Command> child,
                    int exit_status) noexcept override {
    _out << JoinPrinter{c, child, exit_status} << endl;
  }

  virtual void exit(shared_ptr<Command> c, int exit_status) noexcept override {
    _out << ExitPrinter{c, exit_status} << endl;
  }

  /// A wrapper struct used to print SpecialRef IR steps
  struct SpecialRefPrinter {
    shared_ptr<Command> c;
    SpecialRef entity;
    shared_ptr<RefResult> output;

    friend ostream& operator<<(ostream& o, const SpecialRefPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      o << p.output << " = ";
      switch (p.entity) {
        case SpecialRef::stdin:
          o << "STDIN";
          break;

        case SpecialRef::stdout:
          o << "STDOUT";
          break;

        case SpecialRef::stderr:
          o << "STDERR";
          break;

        case SpecialRef::root:
          o << "ROOT";
          break;

        case SpecialRef::cwd:
          o << "CWD";
          break;

        case SpecialRef::launch_exe:
          o << "LAUNCH_EXE";
          break;
      }
      return o;
    }
  };

  /// A wrapper struct used to print PipeRef IR steps
  struct PipeRefPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> read_end;
    shared_ptr<RefResult> write_end;

    friend ostream& operator<<(ostream& o, const PipeRefPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "[" << p.read_end << ", " << p.write_end << "] = PipeRef()";
    }
  };

  /// A wrapper struct used to print FileRef IR steps
  struct FileRefPrinter {
    shared_ptr<Command> c;
    mode_t mode;
    shared_ptr<RefResult> output;

    friend ostream& operator<<(ostream& o, const FileRefPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << p.output << " = FileRef(" << std::oct << p.mode << std::dec << ")";
    }
  };

  /// A wrapper struct used to print SymlinkRef IR steps
  struct SymlinkRefPrinter {
    shared_ptr<Command> c;
    fs::path target;
    shared_ptr<RefResult> output;

    friend ostream& operator<<(ostream& o, const SymlinkRefPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << p.output << " = SymlinkRef(" << p.target << ")";
    }
  };

  /// A wrapper struct used to print DirRef IR steps
  struct DirRefPrinter {
    shared_ptr<Command> c;
    mode_t mode;
    shared_ptr<RefResult> output;

    friend ostream& operator<<(ostream& o, const DirRefPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << p.output << " = DirRef(" << std::oct << p.mode << ")";
    }
  };

  /// A wrapper struct used to print PathRef IR steps
  struct PathRefPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> base;
    fs::path path;
    AccessFlags flags;
    shared_ptr<RefResult> output;

    friend ostream& operator<<(ostream& o, const PathRefPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << p.output << " = PathRef(" << p.base << ", " << p.path << ", " << p.flags << ")";
    }
  };

  /// A wrapper struct used to print ExpectResult IR steps
  struct ExpectResultPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> ref;
    int expected;

    friend ostream& operator<<(ostream& o, const ExpectResultPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "ExpectResult(" << p.ref << ", " << errors[p.expected] << ")";
    }
  };

  /// A wrapper struct used to print MatchMetadata IR steps
  struct MatchMetadataPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> ref;
    shared_ptr<MetadataVersion> expected;

    friend ostream& operator<<(ostream& o, const MatchMetadataPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "MatchMetadata(" << p.ref << ", " << p.expected << ")";
    }
  };

  /// A wrapper struct used to print MatchContent IR steps
  struct MatchContentPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> ref;
    shared_ptr<Version> expected;

    friend ostream& operator<<(ostream& o, const MatchContentPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "MatchContent(" << p.ref << ", " << p.expected << ")";
    }
  };

  /// A wrapper struct used to print UpdateMetadata IR steps
  struct UpdateMetadataPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> ref;
    shared_ptr<MetadataVersion> written;

    friend ostream& operator<<(ostream& o, const UpdateMetadataPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "UpdateMetadata(" << p.ref << ", " << p.written << ")";
    }
  };

  /// A wrapper struct used to print UpdateContent IR steps
  struct UpdateContentPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> ref;
    shared_ptr<Version> written;

    friend ostream& operator<<(ostream& o, const UpdateContentPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "UpdateContent(" << p.ref << ", " << p.written << ")";
    }
  };

  /// A wrapper struct used to print AddEntry IR steps
  struct AddEntryPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> dir;
    fs::path name;
    shared_ptr<RefResult> target;

    friend ostream& operator<<(ostream& o, const AddEntryPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "AddEntry(" << p.dir << ", " << p.name << ", " << p.target << ")";
    }
  };

  /// A wrapper struct used to print RemoveEntry IR steps
  struct RemoveEntryPrinter {
    shared_ptr<Command> c;
    shared_ptr<RefResult> dir;
    fs::path name;
    shared_ptr<RefResult> target;

    friend ostream& operator<<(ostream& o, const RemoveEntryPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "RemoveEntry(" << p.dir << ", " << p.name << ", " << p.target << ")";
    }
  };

  /// A wrapper struct used to print Launch IR steps
  struct LaunchPrinter {
    shared_ptr<Command> c;
    shared_ptr<Command> child;

    friend ostream& operator<<(ostream& o, const LaunchPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "Launch(" << p.child << ")";
    }
  };

  /// A wrapper struct used to print Join IR steps
  struct JoinPrinter {
    shared_ptr<Command> c;
    shared_ptr<Command> child;
    int exit_status;

    friend ostream& operator<<(ostream& o, const JoinPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "Join(" << p.child << ", " << p.exit_status << ")";
    }
  };

  /// A wrapper struct used to print Exit IR steps
  struct ExitPrinter {
    shared_ptr<Command> c;
    int exit_status;

    friend ostream& operator<<(ostream& o, const ExitPrinter& p) noexcept {
      if (p.c) o << p.c << ": ";
      return o << "Exit(" << p.exit_status << ")";
    }
  };

 private:
  ostream& _out;
};
