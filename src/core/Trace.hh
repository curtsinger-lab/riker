#pragma once

#include <filesystem>
#include <fstream>
#include <list>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>

#include <cereal/archives/binary.hpp>

using std::ifstream;
using std::list;
using std::ofstream;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::unique_ptr;

class Command;
class Step;

/**
 * An input trace is a build trace loaded from disk
 */
class InputTrace {
 public:
  using StepList = list<tuple<shared_ptr<Command>, unique_ptr<Step>>>;

  /**
   * Load a trace from a given path. If the trace does not load, fall back to a default trace.
   */
  InputTrace(string filename) noexcept;

  // Disallow copy
  InputTrace(const InputTrace&) = delete;
  InputTrace& operator=(const InputTrace&) = delete;

  // Allow move
  InputTrace(InputTrace&&) = default;
  InputTrace& operator=(InputTrace&&) = default;

  /// Get the list of steps in this trace
  const StepList& getSteps() const noexcept { return _steps; }

  /// Print this trace
  ostream& print(ostream& o) const noexcept;

  friend ostream& operator<<(ostream& o, const InputTrace& t) noexcept { return t.print(o); }

 private:
  /// Initialize the list of steps to a default trace
  void initDefault() noexcept;

  /// A sequence of tuples containing a command and a step performed by that command
  StepList _steps;
};

/**
 * An output trace is used to write a trace to disk
 */
class OutputTrace {
 public:
  /// Create a trace at the given path
  OutputTrace(string filename) noexcept;

  /// Finalize an output trace
  ~OutputTrace() noexcept;

  // Disallow copy
  OutputTrace(const OutputTrace&) = delete;
  OutputTrace& operator=(const OutputTrace&) = delete;

  /// Add a step to this trace
  void addStep(shared_ptr<Command> c, unique_ptr<Step>&& s) noexcept;

 private:
  ofstream _output;
  cereal::BinaryOutputArchive _archive;

  using StepList = list<tuple<shared_ptr<Command>, unique_ptr<Step>>>;
  StepList _steps;
};