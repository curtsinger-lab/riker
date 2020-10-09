AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CONFIG_COMMANDS([foo], [
case $ACTION in
  use)
    echo "$tmp" > tmpdir
    test -d "$tmp" || AC_MSG_ERROR([No $tmp directory])
    echo garbage-in > "$tmp/garbage-out"
    ;;
  corrupt)
    echo "$tmp" > tmpdir
    tmp=./nosuch
    ;;
  *) ;;
esac
])

AC_OUTPUT
AC_STATE_SAVE(after)
