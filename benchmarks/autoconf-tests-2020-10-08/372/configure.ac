AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CHECK_TYPES([int, struct yes_s, struct no_s],,,
		 [struct yes_s { int yes ;} ;])
AC_OUTPUT
AC_STATE_SAVE(after)
