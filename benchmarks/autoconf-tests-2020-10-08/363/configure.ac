AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CHECK_MEMBERS([struct yes_s.yes, struct yes_s.no, struct yes_s.substruct],,,
		   [struct sub { int x; };
		    struct yes_s { int yes; struct sub substruct; };])
AC_OUTPUT
AC_STATE_SAVE(after)
