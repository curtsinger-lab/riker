#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include "runtime/Ref.hh"
#include "util/log.hh"

namespace fs = std::filesystem;

class CommandRun;
class ContentVersion;
class DirVersion;
class MetadataVersion;
class Process;
class Version;

/// The set of possible markings for a command that determine how it is executed during rebuild
enum class RebuildMarking {
  Emulate,  // The Emulate marking indicates that the command has not run, and can be emulated. This
            // is the initial state for all commands. During the rebuild planning process, a command
            // may move from Emulate to MayRun or MustRun.

  MayRun,  // The MayRun marking indicates that the command may need to run. This could be promoted
           // to MustRun during rebuild planning. Commands marked MayRun will not be executed on the
           // current build iteration, but the possibility that they might need to run in the future
           // is important for planning the build correctly.

  MustRun  // The MustRun marking indicates that we are certain a command will need to run, so it
           // will be executed on the next build iteration.
};

/**
 * Predicates are tagged with specific scenarios where they apply.
 *
 * If all of a command's predicates in the Build scenario evaluate to true, the command does not
 * directly observe any change. The same is true for the PostBuild scenario.
 */
enum class Scenario : uint8_t { None = 0, Build = 1, PostBuild = 2, Both = 3 };

inline bool operator&(Scenario s1, Scenario s2) noexcept {
  return (static_cast<uint8_t>(s1) & static_cast<uint8_t>(s2)) != 0;
}

inline Scenario operator|(Scenario s1, Scenario s2) noexcept {
  if (s1 == Scenario::None) {
    return s2;
  } else if (s1 == Scenario::Build) {
    if (s2 == Scenario::None || s2 == Scenario::Build) {
      return Scenario::Build;
    } else {
      return Scenario::Both;
    }
  } else if (s1 == Scenario::PostBuild) {
    if (s2 == Scenario::None || s2 == Scenario::PostBuild) {
      return Scenario::PostBuild;
    } else {
      return Scenario::Both;
    }
  } else {
    return Scenario::Both;
  }
}

inline Scenario& operator|=(Scenario& lhs, Scenario rhs) noexcept {
  lhs = lhs | rhs;
  return lhs;
}

/**
 * Representation of a command that runs as part of the build.
 * Commands correspond to exec() calls during the build process; these are commands we can directly
 * re-execute on a future build. We need to track the paths that commands reference, and their
 * interactions through those paths.
 */
class Command : public std::enable_shared_from_this<Command> {
 private:
  /// Default constructor used to create the null command instance
  Command() noexcept = default;

 public:
  /// The type of a command ID
  using ID = uint32_t;

  /// Create a new command
  Command(std::vector<std::string> args) noexcept;

  /// Declare a destructor
  ~Command() noexcept;

  // Disallow Copy
  Command(const Command&) = delete;
  Command& operator=(const Command&) = delete;

  // Allow Move
  Command(Command&&) noexcept = default;
  Command& operator=(Command&&) noexcept = default;

  /// Create an empty command suitable for use as the root of a build
  static std::shared_ptr<Command> createEmptyCommand() noexcept;

  /// Get a short, printable name for this command
  std::string getShortName(size_t limit = 40) const noexcept;

  /// Get the full name for this command
  std::string getFullName() const noexcept;

  /// Is this command the null command?
  bool isEmptyCommand() const noexcept;

  /// Check if this command has ever executed
  bool hasExecuted() const noexcept { return _executed; }

  /// Record that this command has now been executed
  void setExecuted() noexcept { _executed = true; }

  /// Get the list of arguments this command was started with
  const std::vector<std::string>& getArguments() const noexcept { return _args; }

  /// Get the set of file descriptors set up at the start of this command's run
  const std::map<int, Ref::ID>& getInitialFDs() const noexcept { return _initial_fds; }

  /// Add an initial file descriptor to this command
  void addInitialFD(int fd, Ref::ID ref) noexcept {
    ASSERT(fd >= 0) << "Invalid file descriptor number " << fd << " in " << this;
    _initial_fds.emplace(fd, ref);
  }

  /// Set the initial file descriptors for this command
  void setInitialFDs(std::map<int, Ref::ID> fds) noexcept { _initial_fds = fds; }

  /****** Utility Methods ******/

  /// Print a Command to an output stream
  friend std::ostream& operator<<(std::ostream& o, const Command& c) noexcept {
    if (c.isEmptyCommand()) {
      return o << "[No Command]";
    } else {
      return o << "[Command " << c.getShortName() << "]";
    }
  }

  /// Print a Command* to an output stream
  friend std::ostream& operator<<(std::ostream& o, const Command* c) noexcept {
    if (c == nullptr) return o << "<null Command>";
    return o << *c;
  }

  /****** Rebuild Planning ******/

  /// Finish the current run of this command. This moves the run data to last_run.
  void finishRun() noexcept;

  /// Plan the next build iteration starting with this command
  void planBuild() noexcept;

  /// Check if this command can be emulated for the current build iteration
  bool canEmulate() const noexcept { return _marking != RebuildMarking::MustRun; }

  /// Check if this command may run on a future build iteration
  bool mayRun() const noexcept { return _marking == RebuildMarking::MayRun; }

  /// Check if this command should run on the current build iteration
  bool mustRun() const noexcept { return _marking == RebuildMarking::MustRun; }

  /// Get the marking for this command
  RebuildMarking getMarking() const noexcept { return _marking; }

  /// Directly set a marking on this command without propagating it. Used to mark new commands as
  /// they are launched
  void setMarking(RebuildMarking marking) noexcept { _marking = marking; }

  /// Does this command or any of its descendants need to run? If not, return true.
  bool allFinished() const noexcept;

  /// Get a set of all commands including this one and its descendants
  std::set<std::shared_ptr<Command>> collectCommands() noexcept;

  /// Get a set of all commands that may run from this command and its descendants
  std::set<std::shared_ptr<Command>> collectMayRun() noexcept;

  /// Get a set of all commands that must run from this command and its descendants
  std::set<std::shared_ptr<Command>> collectMustRun() noexcept;

  /****** Types and struct used to track run-specific data ******/

  using WeakCommandSet = std::set<std::weak_ptr<Command>, std::owner_less<std::weak_ptr<Command>>>;

  using InputList =
      std::list<std::tuple<std::shared_ptr<Artifact>,  // The artifact that was accessed
                           std::shared_ptr<Version>,   // The input version
                           std::weak_ptr<Command>>>;   // The command that created theinput

  using OutputList =
      std::list<std::tuple<std::shared_ptr<Artifact>,   // The artifact that was written
                           std::shared_ptr<Version>>>;  // The version written to that artifact

  struct Run {
    /// The command's local references
    std::vector<std::shared_ptr<Ref>> _refs;

    /// This command's use countn for each of its references
    std::vector<size_t> _refs_use_count;

    /// This command's parent, if any
    std::weak_ptr<Command> _parent;

    /// The children launched by this command
    std::list<std::shared_ptr<Command>> _children;

    /// Has this command run been launched by its parent yet? This is set to true whether the launch
    /// is emulated or traced.
    bool _launched = false;

    /// The process this command's run was launched in, or nullptr if there is no process
    std::shared_ptr<Process> _process;

    /// The temporary files used by this command. The value is set to true once the tempfile has
    /// been accessed
    std::map<std::shared_ptr<Artifact>, bool> _tempfiles;

    /// The content this command expects to find in temporary files
    std::map<std::string, std::shared_ptr<ContentVersion>> _tempfile_expected_content;

    /// Path substitutions established by matching this command
    std::map<std::string, std::string> _substitutions;

    /// The exit status for this command
    int _exit_status = -1;

    /// Keep track of the scenarios where this command has observed a change
    Scenario _changed = Scenario::None;

    /// Inputs to this command
    InputList _inputs;

    /// Outputs from this command
    OutputList _outputs;

    /// The set of commands that produce any inputs to this command
    WeakCommandSet _uses_output_from;

    /// The set of commands that produce uncached inputs to this command
    WeakCommandSet _needs_output_from;

    /// The set of commands that use this command's outputs
    WeakCommandSet _output_used_by;

    /// The set of commands that require uncached outputs from this command
    WeakCommandSet _output_needed_by;
  };

  /****** Data for the current run ******/

  /// Create dependencies to prepare this command for execution
  void createLaunchDependencies() noexcept;

  /// This command launched a child command
  void addChild(std::shared_ptr<Command> child) noexcept;

  /// Check if the latest run of this command has been launched yet
  bool isLaunched() noexcept;

  /// Mark the latest run of this command as launched
  void setLaunched(std::shared_ptr<Process> p = nullptr) noexcept;

  /// Get the process this command is running in
  const std::shared_ptr<Process>& getProcess() noexcept;

  /// Get this command's exit status
  int getExitStatus() noexcept;

  /// Set this command's exit status, and record that it has exited
  void setExitStatus(int status) noexcept;

  /// Apply a set of subsitutions to this command's arguments, and save the set for future path
  /// substitutions
  void applySubstitutions(std::map<std::string, std::string> substitutions) noexcept;

  /// Look for a matching path substitution and return the path this command should use
  std::string substitutePath(std::string p) noexcept;

  /// Inform this command that it used a temporary file
  void addTempfile(std::shared_ptr<Artifact> tempfile) noexcept;

  /// Get a reference from this command's reference table
  const std::shared_ptr<Ref>& getRef(Ref::ID id) noexcept;

  /// Store a reference at a known index of this command's local reference table
  void setRef(Ref::ID id, std::shared_ptr<Ref> ref) noexcept;

  /// Store a reference at the next available index of this command's local reference table
  Ref::ID setRef(std::shared_ptr<Ref> ref) noexcept;

  /// Increment the use count for a Ref. Return true if this is the first use of the ref.
  bool usingRef(Ref::ID id) noexcept;

  /// Decrement a use count for a Ref. Return true if this was the last use of the ref.
  bool doneWithRef(Ref::ID id) noexcept;

  /// This command observes a change in a given scenario
  void observeChange(Scenario s) noexcept;

  /// An input to this command did not match the expected version
  void inputChanged(std::shared_ptr<Artifact> artifact,
                    std::shared_ptr<MetadataVersion> observed,
                    std::shared_ptr<MetadataVersion> expected,
                    Scenario scenario) noexcept;

  /// An input to this command did not match the expected version
  void inputChanged(std::shared_ptr<Artifact> artifact,
                    std::shared_ptr<ContentVersion> observed,
                    std::shared_ptr<ContentVersion> expected,
                    Scenario scenario) noexcept;

  /// Track a metadata version input to this command
  void addMetadataInput(std::shared_ptr<Artifact> a,
                        std::shared_ptr<MetadataVersion> v,
                        std::shared_ptr<Command> writer) noexcept;

  /// Track a content version input to this command
  void addContentInput(std::shared_ptr<Artifact> a,
                       std::shared_ptr<ContentVersion> v,
                       std::shared_ptr<Command> writer) noexcept;

  /// Track a directory version input to this command
  void addDirectoryInput(std::shared_ptr<Artifact> a,
                         std::shared_ptr<DirVersion> v,
                         std::shared_ptr<Command> writer) noexcept;

  /// Track a metadata version output from this command
  void addMetadataOutput(std::shared_ptr<Artifact> a, std::shared_ptr<MetadataVersion> v) noexcept;

  /// Track a content version output from this command
  void addContentOutput(std::shared_ptr<Artifact> a, std::shared_ptr<ContentVersion> v) noexcept;

  /// Track a directory version output from this command
  void addDirectoryOutput(std::shared_ptr<Artifact> a, std::shared_ptr<DirVersion> v) noexcept;

  /// An output from this command does not match the on-disk state (checked at the end of the build)
  void outputChanged(std::shared_ptr<Artifact> artifact,
                     std::shared_ptr<ContentVersion> ondisk,
                     std::shared_ptr<ContentVersion> expected) noexcept;

  /****** Data from the previous run ******/

  /// Get this command's list of children
  const std::list<std::shared_ptr<Command>>& getChildren() noexcept;

  /// Get the set of commands that produce inputs to this command
  const WeakCommandSet& getInputProducers() const noexcept;

  /**
   * Does this command match a given set of launch arguments? If so, return a set of substitutions
   * required to make the match work. These substitutions should be applied if the match is used,
   * and paths from the command should be substituted using substitutePath when emulating.
   *
   * \param args  The arguments for the command being launched
   * \returns nullopt if there is not a match, or a map of substitutions required for the match
   */
  std::optional<std::map<std::string, std::string>> tryToMatch(
      const std::vector<std::string>& args,
      const std::map<int, Ref::ID>& fds) const noexcept;

  /// Get the content inputs to this command
  const InputList& getInputs() noexcept;

  /// Get the outputs from this command
  const OutputList& getOutputs() noexcept;

  std::optional<Command::ID> getID(size_t buffer_id) {
    if (_buffer_id == buffer_id) return _id;
    return std::nullopt;
  }

  void setID(size_t buffer_id, Command::ID id) {
    _buffer_id = buffer_id;
    _id = id;
  }

 private:
  /// Assign a marking to this command for the next build. Returns true if this is a new marking.
  bool mark(RebuildMarking marking) noexcept;

 private:
  /// The arguments passed to this command on startup
  std::vector<std::string> _args;

  /// The file descriptor entries populated at the start of this command's execution
  std::map<int, Ref::ID> _initial_fds;

  /// Has this command ever run?
  bool _executed = false;

  /// Transient data for the current run
  Run _current_run;

  /// Transient data for the last run
  Run _previous_run;

  /// The marking state for this command that determines how the command is run
  RebuildMarking _marking = RebuildMarking::Emulate;

  /// Short names of different lengths for this command
  mutable std::map<size_t, std::optional<std::string>> _short_names;

  /// The total count of commands the last time the short name was computed
  mutable size_t _short_name_command_count = 0;

  // ID for this command and the buffer it is identified in
  Command::ID _id;
  size_t _buffer_id;
};
