#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <unistd.h>

#include "data/Trace.hh"
#include "runtime/Build.hh"
#include "ui/commands.hh"
#include "util/Graph.hh"
#include "util/TracePrinter.hh"
#include "util/constants.hh"

using std::ofstream;
using std::string;
using std::stringstream;
using std::vector;

/**
 * Run the `graph` subcommand
 * \param output      The name of the output file, or "-" for stdout
 * \param type        The type of output to produce
 * \param show_all    If true, include system files in the graph
 * \param no_render   If set, generate graphviz source instead of a rendered graph
 */
void do_graph(string output, string type, bool show_all, bool no_render) noexcept {
  // Turn on input/output tracking
  options::track_inputs_outputs = true;

  if (type.empty() && no_render) type = "dot";
  if (type.empty() && !no_render) type = "png";
  if (output.empty()) output = "out." + type;

  // If the output filename is not empty, but has no extension, append one
  if (output.find('.') == string::npos) output += "." + type;

  // Load the build trace
  auto trace = TraceReader::load(constants::DatabaseFilename);
  FAIL_IF(!trace) << "A trace could not be loaded. Run a full build first.";
  auto root_cmd = trace->getRootCommand();

  // Emulate the build
  trace->sendTo(Build());

  // Plan the next build
  root_cmd->planBuild();

  Graph graph(show_all);
  graph.addCommands(root_cmd->collectCommands());

  if (no_render) {
    ofstream f(output);
    f << graph;

  } else {
    // Send graph source to a stringstream
    stringstream ss;
    ss << graph;

    // Create a pipe to send the graphviz output through
    int pipe_fds[2];
    pipe(pipe_fds);

    // Create a child process
    pid_t child = fork();
    if (child == 0) {
      // Running in the child. Close the write end of the pipe and remap the read end to stdin
      close(pipe_fds[1]);
      dup2(pipe_fds[0], STDIN_FILENO);

      // Exec dot
      execlp("dot", "dot", "-T", type.c_str(), "-o", output.c_str(), NULL);

      FAIL << "Failed to render graph with dot. Is graphviz installed?";

    } else {
      // Running in the parent. Close the read end of the pipe
      close(pipe_fds[0]);

      // Write the graphviz output to the pipe
      auto message = ss.str();
      write(pipe_fds[1], message.c_str(), message.size());

      // Close the pipe
      close(pipe_fds[1]);

      // Wait for graphviz to finish
      int status;
      waitpid(child, &status, 0);
    }
  }
}
