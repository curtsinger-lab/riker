AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CHECK_ALIGNOF(char)
AC_CHECK_ALIGNOF(charchar,
[[#include <stddef.h>
#include <stdio.h>
typedef char charchar[2];]])
AC_CHECK_ALIGNOF(charcharchar)

AC_OUTPUT
AC_STATE_SAVE(after)
