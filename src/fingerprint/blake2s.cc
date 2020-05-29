// IMPORTANT NOTE: Keep the feature detection here (SSE vs not) in line with blake2-wrapper.h

#if defined(__SSE__) && defined(__SSE2__)
// TODO: consider ifuncs and dynamic feature detection
#include "../deps/blake2/sse/blake2s.c"
#else
#include "../deps/blake2/ref/blake2s-ref.c"
#endif
