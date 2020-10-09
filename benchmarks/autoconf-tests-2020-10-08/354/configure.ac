AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)

AC_SEARCH_LIBS(cos, oser m ust,,
	     AC_MSG_ERROR([cannot find `cos']))

case "$ac_cv_search_cos" in
  -loser|-lust) AC_MSG_ERROR(jeez, $ac_cv_search_cos must be a cool library!) ;;
esac

AC_OUTPUT
AC_STATE_SAVE(after)
