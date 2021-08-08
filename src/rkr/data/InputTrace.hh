#pragma once

#include <filesystem>
#include <fstream>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <cereal/archives/binary.hpp>

#include "data/IRLoader.hh"
#include "data/IRSink.hh"
#include "data/IRSource.hh"
#include "runtime/Command.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

/**
 * An input trace is a build trace loaded from disk
 */
class InputTrace : public IRSource, public IRLoader {
 private:
  InputTrace(std::string filename, std::vector<std::string> args = {});

 public:
  /**
   * Try to load an input trace.
   *
   * \returns a the loaded trace, or nullptr if one was not available
   */
  static std::unique_ptr<InputTrace> load(std::string filename,
                                          std::vector<std::string> args = {}) noexcept;

  // Disallow copy
  InputTrace(const InputTrace&) = delete;
  InputTrace& operator=(const InputTrace&) = delete;

  /// Send the loaded trace to a trace handler
  virtual void sendTo(IRSink& handler) noexcept override;

  /// Accept r-value reference to a trace handler
  void sendTo(IRSink&& handler) noexcept { return sendTo(handler); }

  /// Get the root command for this trace
  std::shared_ptr<Command> getRootCommand() const noexcept { return IRLoader::getCommand(0); }

 private:
  /// The input stream this trace is read from
  std::ifstream _input;

  /// The binary archive that decodes the loaded trace
  cereal::BinaryInputArchive _archive;

  /// Any extra arguments a user may supply to a buildfile
  std::vector<std::string> _args;
};
