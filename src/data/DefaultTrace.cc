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

shared_ptr<Command> DefaultTrace::sendTo(IRSink& handler) noexcept {
  // Send the root command
  auto root_cmd = Command::getNullCommand();
  handler.start(root_cmd);

  // Create a reference to stdin
  handler.specialRef(root_cmd, SpecialRef::stdin, Ref::Stdin);
  handler.usingRef(root_cmd, Ref::Stdin);

  // Create a reference to stdout
  handler.specialRef(root_cmd, SpecialRef::stdout, Ref::Stdout);
  handler.usingRef(root_cmd, Ref::Stdout);

  // Create a reference to stderr
  handler.specialRef(root_cmd, SpecialRef::stderr, Ref::Stderr);
  handler.usingRef(root_cmd, Ref::Stderr);

  // Set up the reference to the root directory
  handler.specialRef(root_cmd, SpecialRef::root, Ref::Root);
  handler.usingRef(root_cmd, Ref::Root);

  // Set up the reference to the working directory
  handler.specialRef(root_cmd, SpecialRef::cwd, Ref::Cwd);
  handler.usingRef(root_cmd, Ref::Cwd);

  // Set up the reference to the launch executable
  handler.specialRef(root_cmd, SpecialRef::launch_exe, Ref::Exe);
  handler.usingRef(root_cmd, Ref::Exe);

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
  handler.launch(root_cmd, rkr_build_cmd, refs);

  // Finish the trace
  handler.finish();

  // Return the root command
  return root_cmd;
}
