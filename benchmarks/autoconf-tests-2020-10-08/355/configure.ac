AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)

AC_SEARCH_LIBS(printf, oser c ust,,
	     AC_MSG_ERROR([cannot find `printf']))

case "$ac_cv_search_printf" in
  -loser|-lust) AC_MSG_ERROR(jeez, $ac_cv_search_printf must be a cool library!) ;;
  -lc) AC_MSG_ERROR(huh, you need to give -lc?)
esac

AC_OUTPUT
AC_STATE_SAVE(after)
