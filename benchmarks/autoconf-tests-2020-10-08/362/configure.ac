AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CHECK_MEMBER([struct yes_s.yes],
		  [AC_DEFINE([HAVE_STRUCT_YES_S_YES], [1],
			     [Define to 1 if `yes' is a member of `struct yes_s'.])],,
		   [struct sub { int x; };
		    struct yes_s { int yes; struct sub substruct; };])
  AC_CHECK_MEMBER([struct yes_s.no],
		  [AC_DEFINE([HAVE_STRUCT_YES_S_NO], [1],
			     [Define to 1 if `no' is a member of `struct yes_s'.])],,
		   [struct sub { int x; };
		    struct yes_s { int yes; struct sub substruct; };])
  AC_CHECK_MEMBER([struct yes_s.substruct],
		  [AC_DEFINE([HAVE_STRUCT_YES_S_SUBSTRUCT], [1],
			     [Define to 1 if `substruct' is a member of `struct yes_s'.])],,
		   [struct sub { int x; };
		    struct yes_s { int yes; struct sub substruct; };])
AC_OUTPUT
AC_STATE_SAVE(after)
