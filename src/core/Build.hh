#pragma once

#include <array>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/IR.hh"

class Tracer;

using std::array;
using std::make_shared;
using std::string;
using std::vector;

enum class FingerprintLevel { None, Local, All };

class Build {
 public:
  /****** Constructors ******/
  /// Create a build
  Build() {
    _std_pipes[0] = make_shared<Artifact>("stdin");
    _std_pipes[1] = make_shared<Artifact>("stdout");
    _std_pipes[2] = make_shared<Artifact>("stderr");

    _std_refs[0] = make_shared<Pipe>();
    _std_refs[1] = make_shared<Pipe>();
    _std_refs[2] = make_shared<Pipe>();

    map<int, FileDescriptor> default_fds = {{0, FileDescriptor(_std_refs[0], _std_pipes[0], false)},
                                            {1, FileDescriptor(_std_refs[1], _std_pipes[1], true)},
                                            {2, FileDescriptor(_std_refs[2], _std_pipes[2], true)}};

    _root = Command::createRootCommand(default_fds);
  }

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  // Allow Move
  Build(Build&&) = default;
  Build& operator=(Build&&) = default;

  shared_ptr<Command> getRoot() const { return _root; }

  /****** Non-trivial methods ******/

  void run(Rebuild& rebuild, Tracer& tracer);

  template <class Archive>
  friend void serialize(Archive& archive, Build& g, uint32_t version);

 private:
  array<shared_ptr<Artifact>, 3> _std_pipes;
  array<shared_ptr<Reference>, 3> _std_refs;
  shared_ptr<Command> _root;

 public:
  // Global flags to control build behavior

  /// Should the build print commands as they are run?
  static inline bool print_on_run = false;

  /// Is this a dry run?
  static inline bool dry_run = false;

  /****** Optimization ******/
  /// Any command can read the effects of its own writes without versioning or dependencies
  static inline bool ignore_self_reads = true;  // PAPER

  /// Repeated writes by the same command with no interleaved read can be combined
  static inline bool combine_writes = true;  // PAPER

  /// Skip repeated checks of the contents or metadata for the same reference
  static inline bool skip_repeat_checks = true;  // PAPER
};
