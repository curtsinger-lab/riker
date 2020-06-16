#pragma once

#include <memory>

#include <cereal/types/base_class.hpp>

using std::shared_ptr;
using std::string;

class Trace;

/// Try to load a build trace. Exit with an error if loading fails.
shared_ptr<Trace> load_build(string filename, bool default_fallback) noexcept;

/// Save a build trace to a file
void save_build(string filename, shared_ptr<Trace> trace) noexcept;

namespace cereal {
  class access;
}

#define SERIALIZE(...)                        \
  friend class cereal::access;                \
  template <class Archive>                    \
  void serialize(Archive& archive) noexcept { \
    archive(__VA_ARGS__);                     \
  }

#define SERIALIZE_EMPTY()      \
  friend class cereal::access; \
  template <class Archive>     \
  void serialize(Archive& archive) noexcept {}

#define BASE(C) cereal::base_class<C>(this)
