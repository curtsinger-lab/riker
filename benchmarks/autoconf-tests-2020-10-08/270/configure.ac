AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_TRY_LINK_FUNC(printf,,
		  AC_MSG_ERROR([cannot find `printf']))
AC_TRY_LINK_FUNC(Be_doomed_if_your_libc_has_a_function_named_like_this,
		 AC_MSG_ERROR([found a nonexistent function]))
AC_OUTPUT
AC_STATE_SAVE(after)
