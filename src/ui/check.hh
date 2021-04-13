#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace fs = std::filesystem;

void do_build(std::vector<std::string> args, std::optional<fs::path> stats_log_path) noexcept;
