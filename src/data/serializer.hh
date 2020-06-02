#pragma once

#include <memory>

#include <cereal/types/base_class.hpp>

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

/// This dummy type exists only to make sure we have at least one value to serialize in the
/// SERIALIZE macro below. This occupies no space in the serialized output.
struct __dummy_type {
  template <class Archive>
  void serialize(Archive& archive) {}
};

#define SERIALIZE(...)                     \
  friend class cereal::access;             \
  template <class Archive>                 \
  void serialize(Archive& archive) {       \
    __dummy_type __dummy_value;            \
    archive(__dummy_value, ##__VA_ARGS__); \
  }

#define BASE(C) cereal::base_class<C>(this)
