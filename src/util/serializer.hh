#pragma once

#include <memory>

#include <cereal/types/base_class.hpp>

using std::shared_ptr;
using std::string;

class Trace;

/// Load a build trace, or return a default trace if there is no existing trace
shared_ptr<Trace> load_trace(string filename) noexcept;

/// Save a build trace to a file
void save_trace(string filename, shared_ptr<Trace> trace) noexcept;

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
