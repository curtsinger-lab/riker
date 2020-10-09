AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_PROG_CC
case $ac_exeext in
  '' | '.exe' ) ;;
  * ) AC_MSG_ERROR([suspicious executable suffix: $ac_exeext]);;
esac

case $ac_objext in
  'o' | 'obj' ) ;;
  * ) AC_MSG_ERROR([suspicious object suffix: $ac_objext]);;
esac

AC_OUTPUT
AC_STATE_SAVE(after)
