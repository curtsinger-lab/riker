#pragma once

#include <map>
#include <tuple>
#include <vector>

#include <cereal/types/list.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/optional.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/set.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/vector.hpp>

#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"

using std::map;
using std::tuple;
using std::vector;

class InputTrace;

enum : size_t { ArchiveMagic = 0xD0D0D035178357, ArchiveVersion = 101 };

/// A trace is saved on disk as a series of records. Sub-classes are defined in Trace.cc
struct Record {
  Record() noexcept = default;
  virtual ~Record() = default;

  virtual bool isEnd() const noexcept { return false; }

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept = 0;

  template <class Archive>
  void serialize(Archive& archive) {}
};

struct CommandRecord : public Record {
  Command::ID _id;
  vector<string> _args;
  map<int, Command::RefID> _initial_fds;
  bool _executed;

  /// Default constructor for serialization
  CommandRecord() noexcept = default;

  CommandRecord(Command::ID id,
                vector<string> args,
                map<int, Command::RefID> initial_fds,
                bool executed,
                int exit_status) :
      _id(id), _args(args), _initial_fds(initial_fds), _executed(executed) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _id, _args, _initial_fds, _executed);
  }
};

struct SpecialRefRecord : public Record {
  Command::ID _cmd;
  SpecialRef _entity;
  Command::RefID _output;

  /// Default constructor for serialization
  SpecialRefRecord() noexcept = default;

  SpecialRefRecord(Command::ID cmd, SpecialRef entity, Command::RefID output) noexcept :
      _cmd(cmd), _entity(entity), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _entity, _output);
  }
};

struct PipeRefRecord : public Record {
  Command::ID _cmd;
  Command::RefID _read_end;
  Command::RefID _write_end;

  /// Default constructor for serialization
  PipeRefRecord() noexcept = default;

  PipeRefRecord(Command::ID cmd, Command::RefID read_end, Command::RefID write_end) noexcept :
      _cmd(cmd), _read_end(read_end), _write_end(write_end) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _read_end, _write_end);
  }
};

struct FileRefRecord : public Record {
  Command::ID _cmd;
  mode_t _mode;
  Command::RefID _output;

  /// Default constructor for serialization
  FileRefRecord() noexcept = default;

  FileRefRecord(Command::ID cmd, mode_t mode, Command::RefID output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

struct SymlinkRefRecord : public Record {
  Command::ID _cmd;
  fs::path _target;
  Command::RefID _output;

  /// Default constructor for serialization
  SymlinkRefRecord() noexcept = default;

  SymlinkRefRecord(Command::ID cmd, fs::path target, Command::RefID output) noexcept :
      _cmd(cmd), _target(target), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _target, _output);
  }
};

struct DirRefRecord : public Record {
  Command::ID _cmd;
  mode_t _mode;
  Command::RefID _output;

  /// Default constructor for serialization
  DirRefRecord() noexcept = default;

  DirRefRecord(Command::ID cmd, mode_t mode, Command::RefID output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

struct PathRefRecord : public Record {
  Command::ID _cmd;
  Command::RefID _base;
  fs::path _path;
  AccessFlags _flags;
  Command::RefID _output;

  /// Default constructor for serialization
  PathRefRecord() noexcept = default;

  PathRefRecord(Command::ID cmd,
                Command::RefID base,
                fs::path path,
                AccessFlags flags,
                Command::RefID output) noexcept :
      _cmd(cmd), _base(base), _path(path), _flags(flags), _output(output) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _base, _path, _flags, _output);
  }
};

struct UsingRefRecord : public Record {
  Command::ID _cmd;
  Command::RefID _ref;

  /// Default constructor for serialization
  UsingRefRecord() noexcept = default;

  UsingRefRecord(Command::ID cmd, Command::RefID ref) noexcept : _cmd(cmd), _ref(ref) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref);
  }
};

struct DoneWithRefRecord : public Record {
  Command::ID _cmd;
  Command::RefID _ref;

  /// Default constructor for serialization
  DoneWithRefRecord() noexcept = default;

  DoneWithRefRecord(Command::ID cmd, Command::RefID ref) noexcept : _cmd(cmd), _ref(ref) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref);
  }
};

struct CompareRefsRecord : public Record {
  Command::ID _cmd;
  Command::RefID _ref1;
  Command::RefID _ref2;
  RefComparison _type;

  /// Default constructor for serialization
  CompareRefsRecord() noexcept = default;

  CompareRefsRecord(Command::ID cmd,
                    Command::RefID ref1,
                    Command::RefID ref2,
                    RefComparison type) noexcept :
      _cmd(cmd), _ref1(ref1), _ref2(ref2), _type(type) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref1, _ref2, _type);
  }
};

struct ExpectResultRecord : public Record {
  Command::ID _cmd;
  Scenario _scenario;
  Command::RefID _ref;
  int _expected;

  /// Default constructor for serialization
  ExpectResultRecord() noexcept = default;

  ExpectResultRecord(Command::ID cmd, Scenario scenario, Command::RefID ref, int expected) noexcept
      :
      _cmd(cmd), _scenario(scenario), _ref(ref), _expected(expected) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _scenario, _ref, _expected);
  }
};

struct MatchMetadataRecord : public Record {
  Command::ID _cmd;
  Scenario _scenario;
  Command::RefID _ref;
  shared_ptr<MetadataVersion> _version;

  /// Default constructor for serialization
  MatchMetadataRecord() noexcept = default;

  MatchMetadataRecord(Command::ID cmd,
                      Scenario scenario,
                      Command::RefID ref,
                      shared_ptr<MetadataVersion> version) noexcept :
      _cmd(cmd), _scenario(scenario), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _scenario, _ref, _version);
  }
};

struct MatchContentRecord : public Record {
  Command::ID _cmd;
  Scenario _scenario;
  Command::RefID _ref;
  shared_ptr<Version> _version;

  /// Default constructor for serialization
  MatchContentRecord() noexcept = default;

  MatchContentRecord(Command::ID cmd,
                     Scenario scenario,
                     Command::RefID ref,
                     shared_ptr<Version> version) noexcept :
      _cmd(cmd), _scenario(scenario), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _scenario, _ref, _version);
  }
};

struct UpdateMetadataRecord : public Record {
  Command::ID _cmd;
  Command::RefID _ref;
  shared_ptr<MetadataVersion> _version;

  /// Default constructor for serialization
  UpdateMetadataRecord() noexcept = default;

  UpdateMetadataRecord(Command::ID cmd,
                       Command::RefID ref,
                       shared_ptr<MetadataVersion> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

struct UpdateContentRecord : public Record {
  Command::ID _cmd;
  Command::RefID _ref;
  shared_ptr<Version> _version;

  /// Default constructor for serialization
  UpdateContentRecord() noexcept = default;

  UpdateContentRecord(Command::ID cmd, Command::RefID ref, shared_ptr<Version> version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

struct AddEntryRecord : public Record {
  Command::ID _cmd;
  Command::RefID _dir;
  fs::path _name;
  Command::RefID _target;

  /// Default constructor for serialization
  AddEntryRecord() noexcept = default;

  AddEntryRecord(Command::ID cmd, Command::RefID dir, fs::path name, Command::RefID target) noexcept
      :
      _cmd(cmd), _dir(dir), _name(name), _target(target) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _dir, _name, _target);
  }
};

struct RemoveEntryRecord : public Record {
  Command::ID _cmd;
  Command::RefID _dir;
  fs::path _name;
  Command::RefID _target;

  /// Default constructor for serialization
  RemoveEntryRecord() noexcept = default;

  RemoveEntryRecord(Command::ID cmd,
                    Command::RefID dir,
                    fs::path name,
                    Command::RefID target) noexcept :
      _cmd(cmd), _dir(dir), _name(name), _target(target) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _dir, _name, _target);
  }
};

struct LaunchRecord : public Record {
  Command::ID _cmd;
  Command::ID _child;
  list<tuple<Command::RefID, Command::RefID>> _refs;

  /// Default constructor for serialization
  LaunchRecord() noexcept = default;

  LaunchRecord(Command::ID cmd,
               Command::ID child,
               list<tuple<Command::RefID, Command::RefID>> refs) noexcept :
      _cmd(cmd), _child(child), _refs(refs) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _child, _refs);
  }
};

struct JoinRecord : public Record {
  Command::ID _cmd;
  Command::ID _child;
  int _exit_status;

  /// Default constructor for serialization
  JoinRecord() noexcept = default;

  JoinRecord(Command::ID cmd, Command::ID child, int exit_status) noexcept :
      _cmd(cmd), _child(child), _exit_status(exit_status) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _child, _exit_status);
  }
};

struct ExitRecord : public Record {
  Command::ID _cmd;
  int _exit_status;

  /// Default constructor for serialization
  ExitRecord() noexcept = default;

  ExitRecord(Command::ID cmd, int exit_status) noexcept : _cmd(cmd), _exit_status(exit_status) {}

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _exit_status);
  }
};

struct EndRecord : public Record {
  EndRecord() noexcept = default;

  virtual bool isEnd() const noexcept override { return true; }

  virtual void handle(InputTrace& input, TraceHandler& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this));
  }
};

/********** Serialization for additional types **********/

/// Serialization function for struct timespec
template <class Archive>
void serialize(Archive& ar, struct timespec& ts) noexcept {
  ar(ts.tv_sec, ts.tv_nsec);
}

/// Serialization function for std::filesystem::path
namespace std {
  namespace filesystem {
    template <class Archive>
    void load(Archive& ar, path& p) {
      string s;
      ar(s);
      p = s;
    }

    template <class Archive>
    void save(Archive& ar, const path& p) {
      ar(string(p));
    }
  }
}
