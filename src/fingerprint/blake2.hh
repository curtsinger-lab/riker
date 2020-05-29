#pragma once

// IMPORTANT NOTE: Keep the feature detection here (SSE vs not) in line with blake2s-wrapper.cc and
// blake2sp-wrapper.cc

#if defined(__SSE__) && defined(__SSE2__)
#include "../deps/blake2/sse/blake2.h"
#else
#include "../deps/blake2/ref/blake2.h"
#endif
