#pragma once

#include <array>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "core/Artifact.hh"
#include "core/IR.hh"

class Command;
class Tracer;

using std::array;
using std::make_shared;
using std::string;
using std::vector;

enum class FingerprintLevel { None, Local, All };

class Build {
 public:
  /****** Constructors ******/
  Build() {}

  Build(string executable, vector<string> arguments);

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  // Allow Move
  Build(Build&&) = default;
  Build& operator=(Build&&) = default;

  shared_ptr<Command> getRoot() const { return _root; }

  const auto& getDefaultReferences() const { return _default_refs; }

  /****** Non-trivial methods ******/

  void run(Tracer& tracer);

  void check();

  template <class Archive>
  friend void serialize(Archive& archive, Build& g, uint32_t version);

 private:
  shared_ptr<Command> _root;
  array<shared_ptr<Reference>, 3> _default_refs;
  array<shared_ptr<Artifact>, 3> _default_artifacts;

 public:
  // Global flags to control build behavior

  /****** Optimization ******/
  /// Any command can read the effects of its own writes without versioning or dependencies
  static inline bool ignore_self_reads = true;  // PAPER

  /// Repeated writes by the same command with no interleaved read can be combined
  static inline bool combine_writes = true;  // PAPER

  /// Skip repeated checks of the contents or metadata for the same reference
  static inline bool skip_repeat_checks = true;  // PAPER
};
