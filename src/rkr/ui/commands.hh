#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace fs = std::filesystem;

void do_build(std::vector<std::string> args,
              std::optional<fs::path> stats_log_path,
              std::string command_output,
              std::string binary_output,
              bool refresh,
              std::optional<std::string> remote_path,
              std::string remote_flags) noexcept;

void do_run(std::vector<std::string> args,
            std::optional<fs::path> stats_log_path,
            std::string command_output,
            std::string binary_output,
            std::optional<std::string> remaining) noexcept;

void do_audit(std::vector<std::string> args, std::string command_output) noexcept;

void do_check(std::vector<std::string> args) noexcept;

void do_trace(std::vector<std::string> args,
              std::string output,
              std::string trace_binary,
              std::string trace_read) noexcept;

void do_graph(std::vector<std::string> args,
              std::string output,
              std::string type,
              bool show_all,
              bool no_render) noexcept;

void do_stats(std::vector<std::string> args, bool list_artifacts) noexcept;
