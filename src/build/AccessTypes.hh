#pragma once

enum class InputType {
  PathResolution,  // The input is a dependency for path resolution
  Inherited,       // The input is inherited by a command
  Accessed,        // The input is accessed directly
};
