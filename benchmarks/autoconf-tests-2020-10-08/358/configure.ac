AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CONFIG_FILES(config.libobjs:config.in)
AC_REPLACE_FUNCS(printf \
autoconf_ftnirp)
funcs='fprintf fopen autoconf_ftnirpf'
AH_TEMPLATE(HAVE_FOPEN, )
AH_TEMPLATE(HAVE_FPRINTF, )
AH_TEMPLATE(HAVE_AUTOCONF_FTNIRPF, )
AC_REPLACE_FUNCS(\
$funcs)
{ funcs=; unset funcs;}
AC_OUTPUT
AC_STATE_SAVE(after)
