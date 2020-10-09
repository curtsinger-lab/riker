AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_PROG_CC
AC_C_CONST
AC_C_TYPEOF
AC_C_VOLATILE
case $GCC,$ac_cv_c_const,$ac_cv_c_typeof,$ac_cv_c_volatile in
 yes,*no*)
   AC_MSG_ERROR([failed to detect `const', `typeof', or `volatile' support]);;
esac

AC_OUTPUT
AC_STATE_SAVE(after)
