#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include <sys/types.h>
#include <time.h>

#include <cereal/types/array.hpp>
#include <cereal/types/base_class.hpp>
#include <cereal/types/list.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/optional.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/set.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/vector.hpp>

#include "data/AccessFlags.hh"
#include "data/IRLoader.hh"
#include "data/IRSink.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

class ContentVersion;
class MetadataVersion;

enum : size_t { ArchiveMagic = 0xD0D0D035178357, ArchiveVersion = 101 };

/// A trace is saved on disk as a series of records. Sub-classes are defined in Trace.cc
struct Record {
  Record() noexcept = default;
  virtual ~Record() = default;

  virtual bool isEnd() const noexcept { return false; }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept = 0;

  template <class Archive>
  void serialize(Archive& archive) {}
};

struct CommandRecord : public Record {
  Command::ID _id;
  std::vector<std::string> _args;
  std::map<int, Ref::ID> _initial_fds;
  bool _executed;

  /// Default constructor for serialization
  CommandRecord() noexcept = default;

  CommandRecord(Command::ID id,
                std::vector<std::string> args,
                std::map<int, Ref::ID> initial_fds,
                bool executed) :
      _id(id), _args(args), _initial_fds(initial_fds), _executed(executed) {}

  static std::unique_ptr<Record> create(Command::ID id,
                                        std::vector<std::string> args,
                                        std::map<int, Ref::ID> initial_fds,
                                        bool executed) {
    return std::make_unique<CommandRecord>(id, args, initial_fds, executed);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _id, _args, _initial_fds, _executed);
  }
};

struct MetadataVersionRecord : public Record {
  MetadataVersion::ID _id;
  std::shared_ptr<MetadataVersion> _version;

  // Default constructor for serialization
  MetadataVersionRecord() noexcept = default;

  MetadataVersionRecord(MetadataVersion::ID id, std::shared_ptr<MetadataVersion> version) noexcept :
      _id(id), _version(version) {}

  static std::unique_ptr<Record> create(MetadataVersion::ID id,
                                        std::shared_ptr<MetadataVersion> version) {
    return std::make_unique<MetadataVersionRecord>(id, version);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _id, _version);
  }
};

struct ContentVersionRecord : public Record {
  ContentVersion::ID _id;
  std::shared_ptr<ContentVersion> _version;

  // Default constructor for serialization
  ContentVersionRecord() noexcept = default;

  ContentVersionRecord(ContentVersion::ID id, std::shared_ptr<ContentVersion> version) noexcept :
      _id(id), _version(version) {}

  static std::unique_ptr<Record> create(ContentVersion::ID id,
                                        std::shared_ptr<ContentVersion> version) {
    return std::make_unique<ContentVersionRecord>(id, version);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _id, _version);
  }
};

struct SpecialRefRecord : public Record {
  Command::ID _cmd;
  SpecialRef _entity;
  Ref::ID _output;

  /// Default constructor for serialization
  SpecialRefRecord() noexcept = default;

  SpecialRefRecord(Command::ID cmd, SpecialRef entity, Ref::ID output) noexcept :
      _cmd(cmd), _entity(entity), _output(output) {}

  static std::unique_ptr<Record> create(Command::ID cmd, SpecialRef entity, Ref::ID output) {
    return std::make_unique<SpecialRefRecord>(cmd, entity, output);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _entity, _output);
  }
};

struct PipeRefRecord : public Record {
  Command::ID _cmd;
  Ref::ID _read_end;
  Ref::ID _write_end;

  /// Default constructor for serialization
  PipeRefRecord() noexcept = default;

  PipeRefRecord(Command::ID cmd, Ref::ID read_end, Ref::ID write_end) noexcept :
      _cmd(cmd), _read_end(read_end), _write_end(write_end) {}

  static std::unique_ptr<Record> create(Command::ID cmd, Ref::ID read_end, Ref::ID write_end) {
    return std::make_unique<PipeRefRecord>(cmd, read_end, write_end);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _read_end, _write_end);
  }
};

struct FileRefRecord : public Record {
  Command::ID _cmd;
  mode_t _mode;
  Ref::ID _output;

  /// Default constructor for serialization
  FileRefRecord() noexcept = default;

  FileRefRecord(Command::ID cmd, mode_t mode, Ref::ID output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  static std::unique_ptr<Record> create(Command::ID cmd, mode_t mode, Ref::ID output) {
    return std::make_unique<FileRefRecord>(cmd, mode, output);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

struct SymlinkRefRecord : public Record {
  Command::ID _cmd;
  fs::path _target;
  Ref::ID _output;

  /// Default constructor for serialization
  SymlinkRefRecord() noexcept = default;

  SymlinkRefRecord(Command::ID cmd, fs::path target, Ref::ID output) noexcept :
      _cmd(cmd), _target(target), _output(output) {}

  static std::unique_ptr<Record> create(Command::ID cmd, fs::path target, Ref::ID output) {
    return std::make_unique<SymlinkRefRecord>(cmd, target, output);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _target, _output);
  }
};

struct DirRefRecord : public Record {
  Command::ID _cmd;
  mode_t _mode;
  Ref::ID _output;

  /// Default constructor for serialization
  DirRefRecord() noexcept = default;

  DirRefRecord(Command::ID cmd, mode_t mode, Ref::ID output) noexcept :
      _cmd(cmd), _mode(mode), _output(output) {}

  static std::unique_ptr<Record> create(Command::ID cmd, mode_t mode, Ref::ID output) {
    return std::make_unique<DirRefRecord>(cmd, mode, output);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _mode, _output);
  }
};

struct PathRefRecord : public Record {
  Command::ID _cmd;
  Ref::ID _base;
  fs::path _path;
  AccessFlags _flags;
  Ref::ID _output;

  /// Default constructor for serialization
  PathRefRecord() noexcept = default;

  PathRefRecord(Command::ID cmd,
                Ref::ID base,
                fs::path path,
                AccessFlags flags,
                Ref::ID output) noexcept :
      _cmd(cmd), _base(base), _path(path), _flags(flags), _output(output) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Ref::ID base,
                                        fs::path path,
                                        AccessFlags flags,
                                        Ref::ID output) {
    return std::make_unique<PathRefRecord>(cmd, base, path, flags, output);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _base, _path, _flags, _output);
  }
};

struct UsingRefRecord : public Record {
  Command::ID _cmd;
  Ref::ID _ref;

  /// Default constructor for serialization
  UsingRefRecord() noexcept = default;

  UsingRefRecord(Command::ID cmd, Ref::ID ref) noexcept : _cmd(cmd), _ref(ref) {}

  static std::unique_ptr<Record> create(Command::ID cmd, Ref::ID ref) {
    return std::make_unique<UsingRefRecord>(cmd, ref);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref);
  }
};

struct DoneWithRefRecord : public Record {
  Command::ID _cmd;
  Ref::ID _ref;

  /// Default constructor for serialization
  DoneWithRefRecord() noexcept = default;

  DoneWithRefRecord(Command::ID cmd, Ref::ID ref) noexcept : _cmd(cmd), _ref(ref) {}

  static std::unique_ptr<Record> create(Command::ID cmd, Ref::ID ref) {
    return std::make_unique<DoneWithRefRecord>(cmd, ref);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref);
  }
};

struct CompareRefsRecord : public Record {
  Command::ID _cmd;
  Ref::ID _ref1;
  Ref::ID _ref2;
  RefComparison _type;

  /// Default constructor for serialization
  CompareRefsRecord() noexcept = default;

  CompareRefsRecord(Command::ID cmd, Ref::ID ref1, Ref::ID ref2, RefComparison type) noexcept :
      _cmd(cmd), _ref1(ref1), _ref2(ref2), _type(type) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Ref::ID ref1,
                                        Ref::ID ref2,
                                        RefComparison type) {
    return std::make_unique<CompareRefsRecord>(cmd, ref1, ref2, type);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref1, _ref2, _type);
  }
};

struct ExpectResultRecord : public Record {
  Command::ID _cmd;
  Scenario _scenario;
  Ref::ID _ref;
  int _expected;

  /// Default constructor for serialization
  ExpectResultRecord() noexcept = default;

  ExpectResultRecord(Command::ID cmd, Scenario scenario, Ref::ID ref, int expected) noexcept :
      _cmd(cmd), _scenario(scenario), _ref(ref), _expected(expected) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Scenario scenario,
                                        Ref::ID ref,
                                        int expected) {
    return std::make_unique<ExpectResultRecord>(cmd, scenario, ref, expected);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _scenario, _ref, _expected);
  }
};

struct MatchMetadataRecord : public Record {
  Command::ID _cmd;
  Scenario _scenario;
  Ref::ID _ref;
  MetadataVersion::ID _version;

  /// Default constructor for serialization
  MatchMetadataRecord() noexcept = default;

  MatchMetadataRecord(Command::ID cmd,
                      Scenario scenario,
                      Ref::ID ref,
                      MetadataVersion::ID version) noexcept :
      _cmd(cmd), _scenario(scenario), _ref(ref), _version(version) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Scenario scenario,
                                        Ref::ID ref,
                                        MetadataVersion::ID version) {
    return std::make_unique<MatchMetadataRecord>(cmd, scenario, ref, version);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _scenario, _ref, _version);
  }
};

struct MatchContentRecord : public Record {
  Command::ID _cmd;
  Scenario _scenario;
  Ref::ID _ref;
  ContentVersion::ID _version;

  /// Default constructor for serialization
  MatchContentRecord() noexcept = default;

  MatchContentRecord(Command::ID cmd,
                     Scenario scenario,
                     Ref::ID ref,
                     ContentVersion::ID version) noexcept :
      _cmd(cmd), _scenario(scenario), _ref(ref), _version(version) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Scenario scenario,
                                        Ref::ID ref,
                                        ContentVersion::ID version) {
    return std::make_unique<MatchContentRecord>(cmd, scenario, ref, version);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _scenario, _ref, _version);
  }
};

struct UpdateMetadataRecord : public Record {
  Command::ID _cmd;
  Ref::ID _ref;
  MetadataVersion::ID _version;

  /// Default constructor for serialization
  UpdateMetadataRecord() noexcept = default;

  UpdateMetadataRecord(Command::ID cmd, Ref::ID ref, MetadataVersion::ID version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  static std::unique_ptr<Record> create(Command::ID cmd, Ref::ID ref, MetadataVersion::ID version) {
    return std::make_unique<UpdateMetadataRecord>(cmd, ref, version);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

struct UpdateContentRecord : public Record {
  Command::ID _cmd;
  Ref::ID _ref;
  ContentVersion::ID _version;

  /// Default constructor for serialization
  UpdateContentRecord() noexcept = default;

  UpdateContentRecord(Command::ID cmd, Ref::ID ref, ContentVersion::ID version) noexcept :
      _cmd(cmd), _ref(ref), _version(version) {}

  static std::unique_ptr<Record> create(Command::ID cmd, Ref::ID ref, ContentVersion::ID version) {
    return std::make_unique<UpdateContentRecord>(cmd, ref, version);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _ref, _version);
  }
};

struct AddEntryRecord : public Record {
  Command::ID _cmd;
  Ref::ID _dir;
  fs::path _name;
  Ref::ID _target;

  /// Default constructor for serialization
  AddEntryRecord() noexcept = default;

  AddEntryRecord(Command::ID cmd, Ref::ID dir, fs::path name, Ref::ID target) noexcept :
      _cmd(cmd), _dir(dir), _name(name), _target(target) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Ref::ID dir,
                                        fs::path name,
                                        Ref::ID target) {
    return std::make_unique<AddEntryRecord>(cmd, dir, name, target);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _dir, _name, _target);
  }
};

struct RemoveEntryRecord : public Record {
  Command::ID _cmd;
  Ref::ID _dir;
  fs::path _name;
  Ref::ID _target;

  /// Default constructor for serialization
  RemoveEntryRecord() noexcept = default;

  RemoveEntryRecord(Command::ID cmd, Ref::ID dir, fs::path name, Ref::ID target) noexcept :
      _cmd(cmd), _dir(dir), _name(name), _target(target) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Ref::ID dir,
                                        fs::path name,
                                        Ref::ID target) {
    return std::make_unique<RemoveEntryRecord>(cmd, dir, name, target);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _dir, _name, _target);
  }
};

struct LaunchRecord : public Record {
  Command::ID _cmd;
  Command::ID _child;
  std::list<std::tuple<Ref::ID, Ref::ID>> _refs;

  /// Default constructor for serialization
  LaunchRecord() noexcept = default;

  LaunchRecord(Command::ID cmd,
               Command::ID child,
               std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept :
      _cmd(cmd), _child(child), _refs(refs) {}

  static std::unique_ptr<Record> create(Command::ID cmd,
                                        Command::ID child,
                                        std::list<std::tuple<Ref::ID, Ref::ID>> refs) {
    return std::make_unique<LaunchRecord>(cmd, child, refs);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

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

  static std::unique_ptr<Record> create(Command::ID cmd, Command::ID child, int exit_status) {
    return std::make_unique<JoinRecord>(cmd, child, exit_status);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

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

  static std::unique_ptr<Record> create(Command::ID cmd, int exit_status) {
    return std::make_unique<ExitRecord>(cmd, exit_status);
  }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

  template <class Archive>
  void serialize(Archive& archive) {
    archive(cereal::base_class<Record>(this), _cmd, _exit_status);
  }
};

struct EndRecord : public Record {
  EndRecord() noexcept = default;

  virtual bool isEnd() const noexcept override { return true; }

  static std::unique_ptr<Record> create() { return std::make_unique<EndRecord>(); }

  virtual void handle(IRLoader& input, IRSink& handler) noexcept override;

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
