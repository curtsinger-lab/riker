#pragma once

#include <memory>

using std::shared_ptr;
using std::string;

class Command;

/// Try to load a build. Exit with an error if loading fails.
shared_ptr<Command> load_build(string filename, bool default_fallback);

/// Save a build to a file
void save_build(string filename, shared_ptr<Command> root);

namespace cereal {
class access;
}  // namespace cereal

#define SERIALIZE(...)               \
  friend class cereal::access;       \
  template <class Archive>           \
  void serialize(Archive& archive) { \
    archive(__VA_ARGS__);            \
  }
