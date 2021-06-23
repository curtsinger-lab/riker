#include "DefaultTrace.hh"

#include <cstdio>
#include <list>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <unistd.h>

#include "data/IRSink.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

// Create a source for a default starting trace
DefaultTrace::DefaultTrace(vector<string> args) noexcept :
    _root_command(Command::createEmptyCommand()), _args(args) {}

void DefaultTrace::sendTo(IRSink& handler) noexcept {
  // Send the root command
  handler.start(_root_command);

  // Create a reference to stdin
  handler.specialRef(_root_command, SpecialRef::stdin, Ref::Stdin);
  handler.usingRef(_root_command, Ref::Stdin);

  // Create a reference to stdout
  handler.specialRef(_root_command, SpecialRef::stdout, Ref::Stdout);
  handler.usingRef(_root_command, Ref::Stdout);

  // Create a reference to stderr
  handler.specialRef(_root_command, SpecialRef::stderr, Ref::Stderr);
  handler.usingRef(_root_command, Ref::Stderr);

  // Set up the reference to the root directory
  handler.specialRef(_root_command, SpecialRef::root, Ref::Root);
  handler.usingRef(_root_command, Ref::Root);

  // Set up the reference to the working directory
  handler.specialRef(_root_command, SpecialRef::cwd, Ref::Cwd);
  handler.usingRef(_root_command, Ref::Cwd);

  // Set up the reference to the launch executable
  handler.specialRef(_root_command, SpecialRef::launch_exe, Ref::Exe);
  handler.usingRef(_root_command, Ref::Exe);

  // Create a rkr-build command
  auto cmd_args = vector<string>{"rkr-launch"};
  cmd_args.insert(cmd_args.end(), _args.begin(), _args.end());
  auto rkr_build_cmd = make_shared<Command>(cmd_args);

  // Add initial FDs to the root command
  rkr_build_cmd->addInitialFD(STDIN_FILENO, Ref::Stdin);
  rkr_build_cmd->addInitialFD(STDOUT_FILENO, Ref::Stdout);
  rkr_build_cmd->addInitialFD(STDERR_FILENO, Ref::Stderr);

  // Create a mapping for references inherited by the root command
  list<tuple<Ref::ID, Ref::ID>> refs = {{Ref::Stdin, Ref::Stdin},   {Ref::Stdout, Ref::Stdout},
                                        {Ref::Stderr, Ref::Stderr}, {Ref::Root, Ref::Root},
                                        {Ref::Cwd, Ref::Cwd},       {Ref::Exe, Ref::Exe}};

  // Launch the root command
  handler.launch(_root_command, rkr_build_cmd, refs);

  // Finish the trace
  handler.finish();
}
