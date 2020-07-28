#pragma once

enum class InputType {
  PathResolution,  // The input is a dependency for path resolution
  Accessed,        // The input is accessed directly
  Exists,          // The input must exist, but its specific contents do not matter
};
