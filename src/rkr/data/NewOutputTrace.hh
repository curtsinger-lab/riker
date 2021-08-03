#pragma once

#include <cstring>
#include <filesystem>
#include <map>
#include <memory>
#include <string>
#include <tuple>

#include <sys/mman.h>
#include <sys/sendfile.h>
#include <sys/types.h>

#include "data/IRSink.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

/**
 * An output trace is used to write a trace to disk
 */
class NewOutputTrace : public IRSink {
 public:
  /// Create a trace at the given path
  NewOutputTrace(std::optional<std::string> filename = std::nullopt) noexcept {
    // Was a filename provided?
    if (filename.has_value()) {
      // Create a file to hold the trace
      _fd = open(filename.value().c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
      FAIL_IF(_fd == -1) << "Failed to open file " << filename.value() << ": " << ERR;

    } else {
      // Create a file to hold the trace
      _fd = open("/tmp", O_RDWR | O_TMPFILE, 0644);
      FAIL_IF(_fd == -1) << "Failed to open temporary file: " << ERR;
    }

    // Extend the trace to 256MB. Eventually it needs to grow automatically
    _length = 256 * 1024 * 1024;
    int rc = ftruncate(_fd, _length);
    FAIL_IF(rc != 0) << "Failed to extend file to 256MB: " << ERR;

    // Map the file
    _data = (uint8_t*)mmap(nullptr, _length, PROT_READ | PROT_WRITE, MAP_SHARED, _fd, 0);
    FAIL_IF(_data == MAP_FAILED) << "Failed to mmap file: " << ERR;
  }

  /// Close this trace
  virtual ~NewOutputTrace() noexcept override {
    int rc = ftruncate(_fd, _pos);
    WARN_IF(rc != 0) << "Failed to truncate trace: " << ERR;
    rc = close(_fd);
    WARN_IF(rc != 0) << "Failed to close trace: " << ERR;
    rc = munmap(_data, _length);
    WARN_IF(rc != 0) << "Failed to munmap trace: " << ERR;
  }

  // Disallow copy
  NewOutputTrace(const NewOutputTrace&) = delete;
  NewOutputTrace& operator=(const NewOutputTrace&) = delete;

  /// Trace output is starting
  virtual void start(const std::shared_ptr<Command>& root) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
    };
    Record* r = (Record*)&_data[_pos];
    r->tag = RecordType::Start;
    r->cmd = root.get();
    _pos += sizeof(Record);
  }

  /// Trace output is finished
  virtual void finish() noexcept override {
    struct Record {
      RecordType tag;
    };
    Record* r = (Record*)&_data[_pos];
    r->tag = RecordType::Finish;
    _pos += sizeof(Record);
  }

  /// Add a SpecialRef IR step to the output trace
  virtual void specialRef(const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      SpecialRef entity;
      Ref::ID output;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::SpecialRef, command.get(), entity, output};
    _pos += sizeof(Record);
  }

  /// Add a PipeRef IR step to the output trace
  virtual void pipeRef(const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID read_end;
      Ref::ID write_end;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::PipeRef, command.get(), read_end, write_end};
    _pos += sizeof(Record);
  }

  /// Add a FileRef IR step to the output trace
  virtual void fileRef(const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      mode_t mode;
      Ref::ID output;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::FileRef, command.get(), mode, output};
    _pos += sizeof(Record);
  }

  /// Add a SymlinkRef IR step to the output trace
  virtual void symlinkRef(const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override {
    size_t target_id = createPath(target);

    struct Record {
      RecordType tag;
      Command* cmd;
      size_t target;
      Ref::ID output;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::SymlinkRef, command.get(), target_id, output};
    _pos += sizeof(Record);
  }

  /// Add a DirRef IR step to the output trace
  virtual void dirRef(const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      mode_t mode;
      Ref::ID output;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::DirRef, command.get(), mode, output};
    _pos += sizeof(Record);
  }

  /// Add a PathRef IR step to the output trace
  virtual void pathRef(const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override {
    size_t path_id = createPath(path);

    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID base;
      size_t path;
      AccessFlags flags;
      Ref::ID output;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::SymlinkRef, command.get(), base, path_id, flags, output};
    _pos += sizeof(Record);
  }

  /// Add a UsingRef IR step to the output trace
  virtual void usingRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID ref;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::UsingRef, command.get(), ref};
    _pos += sizeof(Record);
  }

  /// Add a DoneWithRef IR step to the output trace
  virtual void doneWithRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID ref;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::DoneWithRef, command.get(), ref};
    _pos += sizeof(Record);
  }

  /// Add a CompareRefs IR step to the output trace
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID ref1;
      Ref::ID ref2;
      RefComparison type;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::CompareRefs, command.get(), ref1, ref2, type};
    _pos += sizeof(Record);
  }

  /// Add a ExpectResult IR step to the output trace
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID ref;
      int expected;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::ExpectResult, command.get(), ref, expected};
    _pos += sizeof(Record);
  }

  /// Add a MatchMetadata IR step to the output trace
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion version) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Scenario scenario;
      Ref::ID ref;
      MetadataVersion version;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::MatchMetadata, command.get(), scenario, ref, version};
    _pos += sizeof(Record);
  }

  /// Add a MatchContent IR step to the output trace
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Scenario scenario;
      Ref::ID ref;
      ContentVersion* version;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::MatchContent, command.get(), scenario, ref, version.get()};
    _pos += sizeof(Record);
  }

  /// Add a UpdateMetadata IR step to the output trace
  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion version) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID ref;
      MetadataVersion version;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::UpdateMetadata, command.get(), ref, version};
    _pos += sizeof(Record);
  }

  /// Add a UpdateContent IR step to the output trace
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Ref::ID ref;
      ContentVersion* version;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::UpdateContent, command.get(), ref, version.get()};
    _pos += sizeof(Record);
  }

  /// Add an AddEntry IR step to the output trace
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept override {
    size_t name_id = createString(name);

    struct Record {
      RecordType tag;
      Command* cmd;
      size_t name;
      Ref::ID target;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::AddEntry, command.get(), name_id, target};
    _pos += sizeof(Record);
  }

  /// Add a RemoveEntry IR step to the output trace
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept override {
    size_t name_id = createString(name);

    struct Record {
      RecordType tag;
      Command* cmd;
      size_t name;
      Ref::ID target;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::RemoveEntry, command.get(), name_id, target};
    _pos += sizeof(Record);
  }

  /// Add a Launch IR step to the output trace
  virtual void launch(const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Command* child;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::Launch, command.get(), child.get()};
    _pos += sizeof(Record);

    // TODO: emit refs
  }

  /// Add a Join IR step to the output trace
  virtual void join(const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      Command* child;
      int exit_status;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::Join, command.get(), child.get(), exit_status};
    _pos += sizeof(Record);
  }

  /// Add a Exit IR step to the output trace
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override {
    struct Record {
      RecordType tag;
      Command* cmd;
      int exit_status;
    };
    Record* r = (Record*)&_data[_pos];
    *r = {RecordType::Exit, command.get(), exit_status};
    _pos += sizeof(Record);
  }

  size_t createPath(const fs::path& path) noexcept { return createString(path.c_str()); }

  size_t createString(const std::string& str) noexcept {
    if (str.size() < sizeof(size_t)) {
      size_t result = 0;
      uint8_t* bytes = (uint8_t*)&result;
      for (int i = 0; i < str.size(); i++) {
        bytes[i] = str[i];
      }
      bytes[sizeof(size_t) - 1] = 0x8 | (uint8_t)str.size();
      return result;

    } else {
      auto iter = _strtab.find(str);
      if (iter == _strtab.end()) {
        size_t result = _pos;

        RecordType* type = (RecordType*)&_data[_pos];
        *type = RecordType::String;
        _pos++;

        memcpy(&_data[_pos], str.data(), str.size());
        _pos += str.size();
        _data[_pos] = '\0';
        _pos++;

        iter = _strtab.emplace_hint(iter, str, result);
      }

      return iter->second;
    }
  }

 private:
  enum class RecordType : uint8_t {
    Start = 99,
    Finish = 1,
    SpecialRef = 2,
    PipeRef = 3,
    FileRef = 4,
    SymlinkRef = 5,
    DirRef = 6,
    PathRef = 7,
    UsingRef = 8,
    DoneWithRef = 9,
    CompareRefs = 10,
    ExpectResult = 11,
    MatchMetadata = 12,
    MatchContent = 13,
    UpdateMetadata = 14,
    UpdateContent = 15,
    AddEntry = 16,
    RemoveEntry = 17,
    Launch = 18,
    Join = 19,
    Exit = 20,
    Command = 21,
    ContentVersion = 22,
    String = 23,
    Path = 24
  };

 private:
  int _fd = -1;
  size_t _length = 0;
  size_t _pos = 0;
  uint8_t* _data = nullptr;

  std::map<std::string, size_t> _strtab;
};
