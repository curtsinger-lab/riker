#pragma once

#include <memory>
#include <ostream>

#include "data/IRSink.hh"
#include "runtime/Ref.hh"
#include "util/wrappers.hh"

using std::endl;
using std::ostream;
using std::shared_ptr;
using std::string;

class Command;
class Ref;

inline static ostream& operator<<(ostream& o, Scenario s) {
  if (s == Scenario::Build) {
    return o << "<build>";
  } else if (s == Scenario::PostBuild) {
    return o << "<post-build>";
  } else {
    return o << "<unknown>";
  }
}

class TracePrinter : public IRSink {
 public:
  /// Create a trace printer that writes to a provided ostream
  TracePrinter(ostream& out) : _out(out) {}

  /// Create a trace printer that writes to a provided ostream (rvalue reference form)
  TracePrinter(ostream&& out) : _out(out) {}

  virtual void specialRef(const shared_ptr<Command>& c,
                          SpecialRef entity,
                          Ref::ID output) noexcept override {
    _out << SpecialRefPrinter{c, entity, output} << endl;
  }

  virtual void pipeRef(const shared_ptr<Command>& c,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override {
    _out << PipeRefPrinter{c, read_end, write_end} << endl;
  }

  virtual void fileRef(const shared_ptr<Command>& c,
                       mode_t mode,
                       Ref::ID output) noexcept override {
    _out << FileRefPrinter{c, mode, output} << endl;
  }

  virtual void symlinkRef(const shared_ptr<Command>& c,
                          fs::path target,
                          Ref::ID output) noexcept override {
    _out << SymlinkRefPrinter{c, target, output} << endl;
  }

  virtual void dirRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept override {
    _out << DirRefPrinter{c, mode, output} << endl;
  }

  virtual void pathRef(const shared_ptr<Command>& c,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override {
    _out << PathRefPrinter{c, base, path, flags, output} << endl;
  }

  virtual void usingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept override {
    _out << UsingRefPrinter{c, ref} << endl;
  }

  virtual void doneWithRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept override {
    _out << DoneWithRefPrinter{c, ref} << endl;
  }

  /// A command depends on the outcome of comparing two different references
  virtual void compareRefs(const shared_ptr<Command>& c,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override {
    _out << CompareRefsPrinter{c, ref1, ref2, type} << endl;
  }

  virtual void expectResult(const shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    _out << ExpectResultPrinter{c, scenario, ref, expected} << endl;
  }

  virtual void matchMetadata(const shared_ptr<Command>& c,
                             Scenario scenario,
                             Ref::ID ref,
                             shared_ptr<MetadataVersion> expected) noexcept override {
    _out << MatchMetadataPrinter{c, scenario, ref, expected} << endl;
  }

  virtual void matchContent(const shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            shared_ptr<ContentVersion> expected) noexcept override {
    _out << MatchContentPrinter{c, scenario, ref, expected} << endl;
  }

  virtual void updateMetadata(const shared_ptr<Command>& c,
                              Ref::ID ref,
                              shared_ptr<MetadataVersion> written) noexcept override {
    _out << UpdateMetadataPrinter{c, ref, written} << endl;
  }

  virtual void updateContent(const shared_ptr<Command>& c,
                             Ref::ID ref,
                             shared_ptr<ContentVersion> written) noexcept override {
    _out << UpdateContentPrinter{c, ref, written} << endl;
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const shared_ptr<Command>& c,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept override {
    _out << AddEntryPrinter{c, dir, name, target} << endl;
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const shared_ptr<Command>& c,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept override {
    _out << RemoveEntryPrinter{c, dir, name, target} << endl;
  }

  virtual void launch(const shared_ptr<Command>& c,
                      const shared_ptr<Command>& child,
                      list<tuple<Ref::ID, Ref::ID>> refs) noexcept override {
    _out << LaunchPrinter{c, child, refs} << endl;
  }

  virtual void join(const shared_ptr<Command>& c,
                    const shared_ptr<Command>& child,
                    int exit_status) noexcept override {
    _out << JoinPrinter{c, child, exit_status} << endl;
  }

  virtual void exit(const shared_ptr<Command>& c, int exit_status) noexcept override {
    _out << ExitPrinter{c, exit_status} << endl;
  }

  /// A wrapper struct used to print SpecialRef IR steps
  struct SpecialRefPrinter {
    shared_ptr<Command> c;
    SpecialRef entity;
    Ref::ID output;

    friend ostream& operator<<(ostream& o, const SpecialRefPrinter& p) noexcept {
      o << p.c << ": r" << p.output << " = ";
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
    Ref::ID read_end;
    Ref::ID write_end;

    friend ostream& operator<<(ostream& o, const PipeRefPrinter& p) noexcept {
      return o << p.c << ": [r" << p.read_end << ", r" << p.write_end << "] = PipeRef()";
    }
  };

  /// A wrapper struct used to print FileRef IR steps
  struct FileRefPrinter {
    shared_ptr<Command> c;
    mode_t mode;
    Ref::ID output;

    friend ostream& operator<<(ostream& o, const FileRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = FileRef(" << std::oct << p.mode << std::dec
               << ")";
    }
  };

  /// A wrapper struct used to print SymlinkRef IR steps
  struct SymlinkRefPrinter {
    shared_ptr<Command> c;
    fs::path target;
    Ref::ID output;

    friend ostream& operator<<(ostream& o, const SymlinkRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = SymlinkRef(" << p.target << ")";
    }
  };

  /// A wrapper struct used to print DirRef IR steps
  struct DirRefPrinter {
    shared_ptr<Command> c;
    mode_t mode;
    Ref::ID output;

    friend ostream& operator<<(ostream& o, const DirRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = DirRef(" << std::oct << p.mode << ")";
    }
  };

  /// A wrapper struct used to print PathRef IR steps
  struct PathRefPrinter {
    shared_ptr<Command> c;
    Ref::ID base;
    fs::path path;
    AccessFlags flags;
    Ref::ID output;

    friend ostream& operator<<(ostream& o, const PathRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = PathRef(r" << p.base << ", " << p.path << ", "
               << p.flags << ")";
    }
  };

  /// A wrapper struct used to print Open IR steps
  struct UsingRefPrinter {
    shared_ptr<Command> c;
    Ref::ID ref;

    friend ostream& operator<<(ostream& o, const UsingRefPrinter& p) noexcept {
      return o << p.c << ": UsingRef(r" << p.ref << ")";
    }
  };

  /// A wrapper struct used to print Close IR steps
  struct DoneWithRefPrinter {
    shared_ptr<Command> c;
    Ref::ID ref;

    friend ostream& operator<<(ostream& o, const DoneWithRefPrinter& p) noexcept {
      return o << p.c << ": DoneWithRef(r" << p.ref << ")";
    }
  };

  /// A wrapper struct used to print CompareRefs IR steps
  struct CompareRefsPrinter {
    shared_ptr<Command> c;
    Ref::ID ref1;
    Ref::ID ref2;
    RefComparison type;

    friend ostream& operator<<(ostream& o, const CompareRefsPrinter& p) noexcept {
      o << p.c << ": ";
      string typestr;
      if (p.type == RefComparison::SameInstance) {
        typestr = "SameInstance";
      } else if (p.type == RefComparison::DifferentInstances) {
        typestr = "DifferentInstances";
      } else {
        typestr = "Unknown";
      }

      return o << "CompareRefs(r" << p.ref1 << ", r" << p.ref2 << ", " << typestr << ")";
    }
  };

  /// A wrapper struct used to print ExpectResult IR steps
  struct ExpectResultPrinter {
    shared_ptr<Command> c;
    Scenario scenario;
    Ref::ID ref;
    int expected;

    friend ostream& operator<<(ostream& o, const ExpectResultPrinter& p) noexcept {
      o << p.c << ": ";
      return o << "ExpectResult(r" << p.ref << ", " << getErrorName(p.expected) << ") "
               << p.scenario;
    }
  };

  /// A wrapper struct used to print MatchMetadata IR steps
  struct MatchMetadataPrinter {
    shared_ptr<Command> c;
    Scenario scenario;
    Ref::ID ref;
    shared_ptr<MetadataVersion> expected;

    friend ostream& operator<<(ostream& o, const MatchMetadataPrinter& p) noexcept {
      return o << p.c << ": MatchMetadata(r" << p.ref << ", " << p.expected << ") " << p.scenario;
    }
  };

  /// A wrapper struct used to print MatchContent IR steps
  struct MatchContentPrinter {
    shared_ptr<Command> c;
    Scenario scenario;
    Ref::ID ref;
    shared_ptr<ContentVersion> expected;

    friend ostream& operator<<(ostream& o, const MatchContentPrinter& p) noexcept {
      return o << p.c << ": MatchContent(r" << p.ref << ", " << p.expected << ") " << p.scenario;
    }
  };

  /// A wrapper struct used to print UpdateMetadata IR steps
  struct UpdateMetadataPrinter {
    shared_ptr<Command> c;
    Ref::ID ref;
    shared_ptr<MetadataVersion> written;

    friend ostream& operator<<(ostream& o, const UpdateMetadataPrinter& p) noexcept {
      return o << p.c << ": UpdateMetadata(r" << p.ref << ", " << p.written << ")";
    }
  };

  /// A wrapper struct used to print UpdateContent IR steps
  struct UpdateContentPrinter {
    shared_ptr<Command> c;
    Ref::ID ref;
    shared_ptr<ContentVersion> written;

    friend ostream& operator<<(ostream& o, const UpdateContentPrinter& p) noexcept {
      return o << p.c << ": UpdateContent(r" << p.ref << ", " << p.written << ")";
    }
  };

  /// A wrapper struct used to print AddEntry IR steps
  struct AddEntryPrinter {
    shared_ptr<Command> c;
    Ref::ID dir;
    fs::path name;
    Ref::ID target;

    friend ostream& operator<<(ostream& o, const AddEntryPrinter& p) noexcept {
      return o << p.c << ": AddEntry(r" << p.dir << ", " << p.name << ", r" << p.target << ")";
    }
  };

  /// A wrapper struct used to print RemoveEntry IR steps
  struct RemoveEntryPrinter {
    shared_ptr<Command> c;
    Ref::ID dir;
    fs::path name;
    Ref::ID target;

    friend ostream& operator<<(ostream& o, const RemoveEntryPrinter& p) noexcept {
      return o << p.c << ": RemoveEntry(r" << p.dir << ", " << p.name << ", r" << p.target << ")";
    }
  };

  /// A wrapper struct used to print Launch IR steps
  struct LaunchPrinter {
    shared_ptr<Command> c;
    shared_ptr<Command> child;
    list<tuple<Ref::ID, Ref::ID>> refs;

    friend ostream& operator<<(ostream& o, const LaunchPrinter& p) noexcept {
      o << p.c << ": Launch(" << p.child << ", [";
      bool first = true;
      for (const auto& [parent_ref_id, child_ref_id] : p.refs) {
        if (!first) o << ", ";
        first = false;
        o << "r" << child_ref_id << "=r" << parent_ref_id;
      }
      return o << "])";
    }
  };

  /// A wrapper struct used to print Join IR steps
  struct JoinPrinter {
    shared_ptr<Command> c;
    shared_ptr<Command> child;
    int exit_status;

    friend ostream& operator<<(ostream& o, const JoinPrinter& p) noexcept {
      return o << p.c << ": Join(" << p.child << ", " << p.exit_status << ")";
    }
  };

  /// A wrapper struct used to print Exit IR steps
  struct ExitPrinter {
    shared_ptr<Command> c;
    int exit_status;

    friend ostream& operator<<(ostream& o, const ExitPrinter& p) noexcept {
      return o << p.c << ": Exit(" << p.exit_status << ")";
    }
  };

 private:
  ostream& _out;
};
