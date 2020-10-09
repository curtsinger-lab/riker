AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_TRY_LINK_FUNC(cos,
		  AC_MSG_ERROR([`cos' is in `libc'], 77))

AC_CHECK_LIB(m, cos,,
	     AC_MSG_ERROR([cannot find `cos' in `libm']))

# No kidding, using variables was broken in 2.50 :(
ac_sin=sin
AC_CHECK_LIB(m, $ac_sin,,
      AC_MSG_ERROR([cannot find `\$ac_sin' (= `$ac_sin') in `libm']))

ac_m=m
AC_CHECK_LIB($ac_m, acos,,
      AC_MSG_ERROR([cannot find `acos' in `\$ac_m' (= `$ac_m')]))

ac_asin=asin
AC_CHECK_LIB($ac_m, $ac_asin,,
      AC_MSG_ERROR([cannot find `\$ac_asin' (= `$ac_asin') in `\$ac_m' (= `$ac_m')]))

# But if the bug is in the caching mechanism, then be sure we
# correctly detect failures.

AC_CHECK_LIB(m, cossack,
	     AC_MSG_ERROR([found `cossack' in `libm']))

# No kidding, using variables was broken in 2.50 :(
ac_sinner=sinner
AC_CHECK_LIB(m, $ac_sinner,
      AC_MSG_ERROR([found `\$ac_sinner' (= `$ac_sinner') in `libm']))

ac_m=m
AC_CHECK_LIB($ac_m, acossack,
      AC_MSG_ERROR([found `acossack' in `\$ac_m' (= `$ac_m')]))

ac_asinner=asinner
AC_CHECK_LIB($ac_m, $ac_asinner,
      AC_MSG_ERROR([found `\$ac_asinner' (= `$ac_asinner') in `\$ac_m' (= `$ac_m')]))


AC_OUTPUT
AC_STATE_SAVE(after)
