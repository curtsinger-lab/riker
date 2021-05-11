#pragma once

#include <filesystem>
#include <memory>
#include <ostream>

#include "data/IRSink.hh"
#include "runtime/Ref.hh"
#include "util/wrappers.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

class Command;
class Ref;

inline static std::ostream& operator<<(std::ostream& o, Scenario s) {
  if (s == Scenario::None) {
    return o << "[no scenario]";
  } else if (s == Scenario::Build) {
    return o << "[build]";
  } else if (s == Scenario::PostBuild) {
    return o << "[post-build]";
  } else if (s == Scenario::Both) {
    return o << "[build, post-build]";
  } else {
    return o << "[unknown scenario]";
  }
}

class TracePrinter : public IRSink {
 public:
  /// Create a trace printer that writes to a provided ostream
  TracePrinter(std::ostream& out) : _out(out) {}

  /// Create a trace printer that writes to a provided ostream (rvalue reference form)
  TracePrinter(std::ostream&& out) : _out(out) {}

  virtual void specialRef(const std::shared_ptr<Command>& c,
                          SpecialRef entity,
                          Ref::ID output) noexcept override {
    _out << SpecialRefPrinter{c, entity, output} << std::endl;
  }

  virtual void pipeRef(const std::shared_ptr<Command>& c,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override {
    _out << PipeRefPrinter{c, read_end, write_end} << std::endl;
  }

  virtual void fileRef(const std::shared_ptr<Command>& c,
                       mode_t mode,
                       Ref::ID output) noexcept override {
    _out << FileRefPrinter{c, mode, output} << std::endl;
  }

  virtual void symlinkRef(const std::shared_ptr<Command>& c,
                          fs::path target,
                          Ref::ID output) noexcept override {
    _out << SymlinkRefPrinter{c, target, output} << std::endl;
  }

  virtual void dirRef(const std::shared_ptr<Command>& c,
                      mode_t mode,
                      Ref::ID output) noexcept override {
    _out << DirRefPrinter{c, mode, output} << std::endl;
  }

  virtual void pathRef(const std::shared_ptr<Command>& c,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override {
    _out << PathRefPrinter{c, base, path, flags, output} << std::endl;
  }

  virtual void usingRef(const std::shared_ptr<Command>& c, Ref::ID ref) noexcept override {
    _out << UsingRefPrinter{c, ref} << std::endl;
  }

  virtual void doneWithRef(const std::shared_ptr<Command>& c, Ref::ID ref) noexcept override {
    _out << DoneWithRefPrinter{c, ref} << std::endl;
  }

  /// A command depends on the outcome of comparing two different references
  virtual void compareRefs(const std::shared_ptr<Command>& c,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override {
    _out << CompareRefsPrinter{c, ref1, ref2, type} << std::endl;
  }

  virtual void expectResult(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    _out << ExpectResultPrinter{c, scenario, ref, expected} << std::endl;
  }

  virtual void matchMetadata(const std::shared_ptr<Command>& c,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion expected) noexcept override {
    _out << MatchMetadataPrinter{c, scenario, ref, expected} << std::endl;
  }

  virtual void matchContent(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override {
    _out << MatchContentPrinter{c, scenario, ref, expected} << std::endl;
  }

  virtual void updateMetadata(const std::shared_ptr<Command>& c,
                              Ref::ID ref,
                              MetadataVersion written) noexcept override {
    _out << UpdateMetadataPrinter{c, ref, written} << std::endl;
  }

  virtual void updateContent(const std::shared_ptr<Command>& c,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> written) noexcept override {
    _out << UpdateContentPrinter{c, ref, written} << std::endl;
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const std::shared_ptr<Command>& c,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept override {
    _out << AddEntryPrinter{c, dir, name, target} << std::endl;
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const std::shared_ptr<Command>& c,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept override {
    _out << RemoveEntryPrinter{c, dir, name, target} << std::endl;
  }

  virtual void launch(const std::shared_ptr<Command>& c,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override {
    _out << LaunchPrinter{c, child, refs} << std::endl;
  }

  virtual void join(const std::shared_ptr<Command>& c,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override {
    _out << JoinPrinter{c, child, exit_status} << std::endl;
  }

  virtual void exit(const std::shared_ptr<Command>& c, int exit_status) noexcept override {
    _out << ExitPrinter{c, exit_status} << std::endl;
  }

  /// A wrapper struct used to print SpecialRef IR steps
  struct SpecialRefPrinter {
    std::shared_ptr<Command> c;
    SpecialRef entity;
    Ref::ID output;

    friend std::ostream& operator<<(std::ostream& o, const SpecialRefPrinter& p) noexcept {
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
    std::shared_ptr<Command> c;
    Ref::ID read_end;
    Ref::ID write_end;

    friend std::ostream& operator<<(std::ostream& o, const PipeRefPrinter& p) noexcept {
      return o << p.c << ": [r" << p.read_end << ", r" << p.write_end << "] = PipeRef()";
    }
  };

  /// A wrapper struct used to print FileRef IR steps
  struct FileRefPrinter {
    std::shared_ptr<Command> c;
    mode_t mode;
    Ref::ID output;

    friend std::ostream& operator<<(std::ostream& o, const FileRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = FileRef(" << std::oct << p.mode << std::dec
               << ")";
    }
  };

  /// A wrapper struct used to print SymlinkRef IR steps
  struct SymlinkRefPrinter {
    std::shared_ptr<Command> c;
    fs::path target;
    Ref::ID output;

    friend std::ostream& operator<<(std::ostream& o, const SymlinkRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = SymlinkRef(" << p.target << ")";
    }
  };

  /// A wrapper struct used to print DirRef IR steps
  struct DirRefPrinter {
    std::shared_ptr<Command> c;
    mode_t mode;
    Ref::ID output;

    friend std::ostream& operator<<(std::ostream& o, const DirRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = DirRef(" << std::oct << p.mode << ")";
    }
  };

  /// A wrapper struct used to print PathRef IR steps
  struct PathRefPrinter {
    std::shared_ptr<Command> c;
    Ref::ID base;
    fs::path path;
    AccessFlags flags;
    Ref::ID output;

    friend std::ostream& operator<<(std::ostream& o, const PathRefPrinter& p) noexcept {
      return o << p.c << ": r" << p.output << " = PathRef(r" << p.base << ", " << p.path << ", "
               << p.flags << ")";
    }
  };

  /// A wrapper struct used to print Open IR steps
  struct UsingRefPrinter {
    std::shared_ptr<Command> c;
    Ref::ID ref;

    friend std::ostream& operator<<(std::ostream& o, const UsingRefPrinter& p) noexcept {
      return o << p.c << ": UsingRef(r" << p.ref << ")";
    }
  };

  /// A wrapper struct used to print Close IR steps
  struct DoneWithRefPrinter {
    std::shared_ptr<Command> c;
    Ref::ID ref;

    friend std::ostream& operator<<(std::ostream& o, const DoneWithRefPrinter& p) noexcept {
      return o << p.c << ": DoneWithRef(r" << p.ref << ")";
    }
  };

  /// A wrapper struct used to print CompareRefs IR steps
  struct CompareRefsPrinter {
    std::shared_ptr<Command> c;
    Ref::ID ref1;
    Ref::ID ref2;
    RefComparison type;

    friend std::ostream& operator<<(std::ostream& o, const CompareRefsPrinter& p) noexcept {
      o << p.c << ": ";
      std::string typestr;
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
    std::shared_ptr<Command> c;
    Scenario scenario;
    Ref::ID ref;
    int expected;

    friend std::ostream& operator<<(std::ostream& o, const ExpectResultPrinter& p) noexcept {
      o << p.c << ": ";
      return o << "ExpectResult(r" << p.ref << ", " << getErrorName(p.expected) << ") "
               << p.scenario;
    }
  };

  /// A wrapper struct used to print MatchMetadata IR steps
  struct MatchMetadataPrinter {
    std::shared_ptr<Command> c;
    Scenario scenario;
    Ref::ID ref;
    MetadataVersion expected;

    friend std::ostream& operator<<(std::ostream& o, const MatchMetadataPrinter& p) noexcept {
      return o << p.c << ": MatchMetadata(r" << p.ref << ", " << p.expected << ") " << p.scenario;
    }
  };

  /// A wrapper struct used to print MatchContent IR steps
  struct MatchContentPrinter {
    std::shared_ptr<Command> c;
    Scenario scenario;
    Ref::ID ref;
    std::shared_ptr<ContentVersion> expected;

    friend std::ostream& operator<<(std::ostream& o, const MatchContentPrinter& p) noexcept {
      return o << p.c << ": MatchContent(r" << p.ref << ", " << p.expected << ") " << p.scenario;
    }
  };

  /// A wrapper struct used to print UpdateMetadata IR steps
  struct UpdateMetadataPrinter {
    std::shared_ptr<Command> c;
    Ref::ID ref;
    MetadataVersion written;

    friend std::ostream& operator<<(std::ostream& o, const UpdateMetadataPrinter& p) noexcept {
      return o << p.c << ": UpdateMetadata(r" << p.ref << ", " << p.written << ")";
    }
  };

  /// A wrapper struct used to print UpdateContent IR steps
  struct UpdateContentPrinter {
    std::shared_ptr<Command> c;
    Ref::ID ref;
    std::shared_ptr<ContentVersion> written;

    friend std::ostream& operator<<(std::ostream& o, const UpdateContentPrinter& p) noexcept {
      return o << p.c << ": UpdateContent(r" << p.ref << ", " << p.written << ")";
    }
  };

  /// A wrapper struct used to print AddEntry IR steps
  struct AddEntryPrinter {
    std::shared_ptr<Command> c;
    Ref::ID dir;
    std::string name;
    Ref::ID target;

    friend std::ostream& operator<<(std::ostream& o, const AddEntryPrinter& p) noexcept {
      return o << p.c << ": AddEntry(r" << p.dir << ", \"" << p.name << "\", r" << p.target << ")";
    }
  };

  /// A wrapper struct used to print RemoveEntry IR steps
  struct RemoveEntryPrinter {
    std::shared_ptr<Command> c;
    Ref::ID dir;
    std::string name;
    Ref::ID target;

    friend std::ostream& operator<<(std::ostream& o, const RemoveEntryPrinter& p) noexcept {
      return o << p.c << ": RemoveEntry(r" << p.dir << ", \"" << p.name << "\", r" << p.target
               << ")";
    }
  };

  /// A wrapper struct used to print Launch IR steps
  struct LaunchPrinter {
    std::shared_ptr<Command> c;
    std::shared_ptr<Command> child;
    std::list<std::tuple<Ref::ID, Ref::ID>> refs;

    friend std::ostream& operator<<(std::ostream& o, const LaunchPrinter& p) noexcept {
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
    std::shared_ptr<Command> c;
    std::shared_ptr<Command> child;
    int exit_status;

    friend std::ostream& operator<<(std::ostream& o, const JoinPrinter& p) noexcept {
      return o << p.c << ": Join(" << p.child << ", " << p.exit_status << ")";
    }
  };

  /// A wrapper struct used to print Exit IR steps
  struct ExitPrinter {
    std::shared_ptr<Command> c;
    int exit_status;

    friend std::ostream& operator<<(std::ostream& o, const ExitPrinter& p) noexcept {
      return o << p.c << ": Exit(" << p.exit_status << ")";
    }
  };

 private:
  std::ostream& _out;
};
