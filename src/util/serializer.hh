#pragma once

#include <cereal/access.hpp>  // IWYU pragma: export
#include <cereal/types/base_class.hpp>

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
