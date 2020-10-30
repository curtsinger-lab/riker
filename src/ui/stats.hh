#pragma once

#include <string>

#include "runtime/Build.hh"

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
 * Generate header string for stats CSV.
 */
void stats_header(string& header) {
  bool needs_prefix = false;
  for (string phase : {"pre", "rebuild", "post"}) {
    if (needs_prefix) header += ",";
    header += pre(phase, "num_commands") + ",";
    header += pre(phase, "num_traced_commmands") + ",";
    header += pre(phase, "num_emulated_commands") + ",";
    header += pre(phase, "num_steps") + ",";
    header += pre(phase, "num_emulated_steps") + ",";
    header += pre(phase, "num_artifacts") + ",";
    header += pre(phase, "num_versions");
    if (!needs_prefix) needs_prefix = true;
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
void gather_stats(optional<fs::path> p, Build& build, optional<string>& stats_opt, string phase) {
  if (p.has_value()) {
    // if the stats string is already defined, start with a comma
    string prefix = "";
    if (stats_opt) {
      prefix = ",";
    }

    // load the environment to count some things
    auto final_env = build.getEnvironment();

    // Count the number of artifact versions
    size_t version_count = 0;
    for (const auto& artifact : final_env->getArtifacts()) {
      version_count += artifact->getVersionCount();
    }

    if (!stats_opt.has_value()) {
      stats_opt = "";
    }

    stats_opt.value() += prefix + q(std::to_string(build.getCommandCount())) + "," +
                         q(std::to_string(build.getTracedCommandCount())) + "," +
                         q(std::to_string(build.getEmulatedCommandCount())) + "," +
                         q(std::to_string(build.getStepCount())) + "," +
                         q(std::to_string(build.getEmulatedStepCount())) + "," +
                         q(std::to_string(final_env->getArtifacts().size())) + "," +
                         q(std::to_string(version_count));
  }
}