// IMPORTANT NOTE: Keep the feature detection here (SSE vs not) in line with blake2s-wrapper.cc and blake2sp-wrapper.cc

#if defined(__SSE__) && defined(__SSE2__)
#include "../blake2/sse/blake2.h"
#else
#include "../blake2/ref/blake2.h"
#endif
