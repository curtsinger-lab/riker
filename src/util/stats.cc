#include "stats.hh"

#include <chrono>
#include <fstream>
#include <optional>
#include <string>

using std::endl;
using std::fstream;
using std::optional;
using std::string;
using std::to_string;

namespace fs = std::filesystem;

#define HEADER                                                                         \
  {                                                                                    \
    "phase", "emulated_commands", "traced_commands", "emulated_steps", "traced_steps", \
        "artifacts", "versions", "ptrace_stops", "syscalls", "elapsed_ns"              \
  }

/**
 * Quote string.
 */
string q(string s) {
  return "\"" + s + "\"";
}

/**
 * Generate a CSV row given an array.
 */
void generate_row(string data[], int len, string& row) {
  for (int i = 0; i < len - 1; i++) {
    row += data[i] + ",";
  }
  row += data[len - 1];
}

/**
 * Generate header string for stats CSV. Writes through given string reference.
 */
int stats_header(string& header) {
  string stats[] = HEADER;
  int stats_len = sizeof(stats) / sizeof(*stats);
  string arr[stats_len];

  // every statistic is measured once in each phase,
  // so create an array containing all the strings and
  // then call generate_row
  int i = 0;
  for (string colname : HEADER) {
    arr[i] = q(colname);
    i++;
  }
  generate_row(arr, stats_len, header);

  return stats_len;
}

/**
 * Write stats to CSV.
 */
void write_stats(optional<fs::path> p, optional<string> stats) {
  if (p.has_value()) {  // if the log doesn't exist, write a header
    if (!std::filesystem::exists(p.value())) {
      string header = "";
      stats_header(header);
      std::ofstream output(p.value());
      output << header << endl << stats.value() << endl;
      output.close();
    } else {  // otherwise, append
      std::ofstream output;
      output.open(p.value(), std::ostream::out | std::ostream::app);
      output << stats.value() << endl;
      output.close();
    }
  }
}

/**
 * Generate a stats row fragment in CSV format
 */
void gather_stats(optional<fs::path> p, optional<string>& stats_opt, int phase) {
  if (p.has_value()) {
    // if the stats string is already defined, start with a newline
    string prefix = "";
    if (stats_opt) {
      prefix = "\n";
    }

    if (!stats_opt.has_value()) {
      stats_opt = "";
    }

    auto end_time = std::chrono::high_resolution_clock::now();

    // Add data to the stats value
    stats_opt.value() += prefix;
    stats_opt.value() += q(to_string(phase)) + ",";
    stats_opt.value() += q(to_string(stats::emulated_commands)) + ",";
    stats_opt.value() += q(to_string(stats::traced_commands)) + ",";
    stats_opt.value() += q(to_string(stats::emulated_steps)) + ",";
    stats_opt.value() += q(to_string(stats::traced_steps)) + ",";
    stats_opt.value() += q(to_string(stats::artifacts)) + ",";
    stats_opt.value() += q(to_string(stats::versions)) + ",";
    stats_opt.value() += q(to_string(stats::ptrace_stops)) + ",";
    stats_opt.value() += q(std::to_string(stats::syscalls)) + ",";
    stats_opt.value() += q(std::to_string((end_time - stats::start_time).count()));
  }
}
