#pragma once

#include <array>
#include <memory>

class Artifact;
class Command;
class Rebuild;
class Reference;
class Tracer;

using std::array;
using std::make_shared;
using std::shared_ptr;

enum class FingerprintLevel { None, Local, All };

class Build {
 public:
  /****** Constructors ******/
  /// Create a build
  Build();

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
