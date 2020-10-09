AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_C_CONST
AC_CHECK_SIZEOF([struct x], [], [struct x { char c; int x; };])
AC_CHECK_SIZEOF([const struct x], [], [struct x { const char *p; int x; };])
AC_CHECK_SIZEOF([struct nosuchstruct])
# Taken from autoconf.texi:Generic Compiler Characteristics.
AC_CHECK_SIZEOF([int *])

AC_OUTPUT
AC_STATE_SAVE(after)
