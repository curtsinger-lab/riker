#pragma once

#include <memory>

#include <cereal/types/base_class.hpp>

using std::shared_ptr;
using std::string;

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
