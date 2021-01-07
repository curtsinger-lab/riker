#include "stats.hh"

// IWYU pragma: no_include <initializer_list>
// IWYU pragma: no_include <type_traits>

#include <chrono>
#include <fstream>
#include <string>

using std::endl;
using std::fstream;
using std::optional;
using std::string;
using std::to_string;

namespace fs = std::filesystem;

#define PHASES \
  { "pre", "rebuild", "post" }

#define STATISTICS                                                                                 \
  {                                                                                                \
    "num_commands", "num_traced_commmands", "num_emulated_commands", "num_steps",                  \
        "num_emulated_steps", "num_artifacts", "num_versions", "num_ptrace_stops", "num_syscalls", \
        "elapsed_ns"                                                                               \
  }

/**
 * Quote string.
 */
string q(string s) {
  return "\"" + s + "\"";
}

/**
 * Prefix string with string and then quote.
 */
string pre(string prefix, string s) {
  return q(prefix + "_" + s);
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
  string stats[] = STATISTICS;
  int stats_len = sizeof(stats) / sizeof(*stats);
  string phases[] = PHASES;
  int phases_len = sizeof(phases) / sizeof(*phases);
  string arr[phases_len * stats_len];

  // every statistic is measured once in each phase,
  // so create an array containing all the strings and
  // then call generate_row
  int i = 0;
  for (string phase : PHASES) {
    for (string colname : STATISTICS) {
      arr[i] = pre(phase, colname);
      i++;
    }
  }
  generate_row(arr, phases_len * stats_len, header);

  return phases_len * stats_len;
}

/**
 * Generate an empty row for stats CSV. This is to ensure that non-dodo
 * benchmarks output a CSV with the same format.
 */
void write_empty_stats(optional<fs::path> p) {
  if (p.has_value()) {
    // create empty row data
    string header;
    int cols = stats_header(header);
    string data[cols];
    for (int i = 0; i < cols; i++) data[i] = q("0");
    string row;
    generate_row(data, cols, row);

    if (!std::filesystem::exists(p.value())) {  // if the log doesn't exist, write a header
      std::ofstream output(p.value());
      output << header << endl << row << endl;
      output.close();
    } else {  // otherwise, append
      std::ofstream output;
      output.open(p.value(), std::ostream::out | std::ostream::app);
      output << row << endl;
      output.close();
    }
  }
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
void gather_stats(optional<fs::path> p, optional<string>& stats_opt, string phase) {
  if (p.has_value()) {
    // if the stats string is already defined, start with a comma
    string prefix = "";
    if (stats_opt) {
      prefix = ",";
    }

    if (!stats_opt.has_value()) {
      stats_opt = "";
    }

    auto end_time = std::chrono::high_resolution_clock::now();

    // Add data to the stats value
    stats_opt.value() += prefix;
    stats_opt.value() += q(to_string(stats::emulated_commands + stats::traced_commands)) + ",";
    stats_opt.value() += q(to_string(stats::traced_commands)) + ",";
    stats_opt.value() += q(to_string(stats::emulated_commands)) + ",";
    stats_opt.value() += q(to_string(stats::emulated_steps + stats::traced_steps)) + ",";
    stats_opt.value() += q(to_string(stats::emulated_steps)) + ",";
    stats_opt.value() += q(to_string(stats::artifacts)) + ",";
    stats_opt.value() += q(to_string(stats::versions)) + ",";
    stats_opt.value() += q(to_string(stats::ptrace_stops)) + ",";
    stats_opt.value() += q(std::to_string(stats::syscalls)) + ",";
    stats_opt.value() += q(std::to_string((end_time - stats::start_time).count()));
  }
}
