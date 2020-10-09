AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([test_blah], [1.24-b])
## Test that the generated directory name is well-formed:
if test `echo "$ERLANG_INSTALL_LIB_DIR_test_blah" | sed -e 's/^.*\///'` != "test_blah-1.24-b"; then
	AC_MSG_ERROR([incorrect ERLANG_INSTALL_LIB_DIR_test_blah variable])
fi

AC_OUTPUT
AC_STATE_SAVE(after)
