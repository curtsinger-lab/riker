// IMPORTANT NOTE: Keep the feature detection here (SSE vs not) in line with blake2-wrapper.h

#if defined(__SSE__) && defined(__SSE2__)
// TODO: consider ifuncs and dynamic feature detection

#if defined(__SSSE3__)
#define HAVE_SSSE3
#endif
#if defined(__SSE4_1__)
#define HAVE_SSE41
#endif
#if defined(__AVX__)
#define HAVE_AVX
#endif
#if defined(__XOP__)
#define HAVE_XOP
#endif

#include "../blake2/sse/blake2sp.c"
#else
#include "../blake2/ref/blake2sp-ref.c"
#endif
