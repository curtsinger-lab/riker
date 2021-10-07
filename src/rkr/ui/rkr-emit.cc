#include <filesystem>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "data/Trace.hh"
#include "ui/commands.hh"

namespace fs = std::filesystem;

using std::make_shared;
using std::string;

/**
 * Run the `emit` subcommand.
 */
void do_emit(string output_file) noexcept {
  // Create a TraceWriter to write the trace
  TraceWriter out(output_file);

  // Every build needs a root command, so make one.
  auto root_cmd = make_shared<Command>();

  // The trace always begins with a start call
  out.start(root_cmd);

  // Create a reference to the current directory. We'll call that ref 0.
  out.specialRef(root_cmd, SpecialRef::cwd, 0);

  // Now we'll create a reference to a file named "A" in the current directory. Call that ref 1.
  out.pathRef(root_cmd, 0, "A", NoAccess, 1);

  // Let's create a new link to "A" named "B" in the current directory
  out.addEntry(root_cmd, 0, "B", 1);

  // Now finish the trace
  out.finish();
}
