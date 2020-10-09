AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
touch at-exists1 at-exists2
ac_exists2=at-exists2
ac_missing2=at-missing2
AC_CHECK_FILES(at-exists1 at-missing1 $ac_exists2 $ac_missing2)
rm at-exists1 at-exists2
AC_OUTPUT
AC_STATE_SAVE(after)
