AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(charchar,,
[[#include <stdio.h>
typedef char charchar[2];]])
AC_CHECK_SIZEOF(charcharchar)

AC_OUTPUT
AC_STATE_SAVE(after)
