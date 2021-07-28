#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <sys/mman.h>

#include "artifacts/Artifact.hh"
#include "data/InputTrace.hh"
#include "data/OutputTrace.hh"
#include "data/ReadWriteCombiner.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "ui/commands.hh"
#include "util/Graph.hh"
#include "util/TracePrinter.hh"
#include "util/constants.hh"
#include "util/stats.hh"

using std::cout;
using std::endl;
using std::ofstream;
using std::string;
using std::stringstream;
using std::vector;

/**
 * Run the `bench` subcommand
 */
void do_bench(std::vector<std::string> args) noexcept {
  // Load the serialized build trace
  auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  trace->sendTo(IRBuffer());

  /*int fd = open(constants::DatabaseFilename.c_str(), O_RDONLY);
  struct stat info;
  fstat(fd, &info);
  uint8_t* p = (uint8_t*)mmap(nullptr, info.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

  FAIL_IF(p == MAP_FAILED) << "Failed to mmap file: " << ERR;

  uint8_t val = 0;
  size_t bytes = 0;
  for (size_t i = 0; i < info.st_size; i++) {
    val ^= p[i];
    bytes++;
  }*/
}
