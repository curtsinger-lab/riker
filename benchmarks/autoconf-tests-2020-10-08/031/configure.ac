m4_define([MACRO],
[[#define STRING "hello, world\n"
]])

AC_INIT
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
	       MACRO]], [[printf (STRING);]])],[],[AS_EXIT([1])])
AC_OUTPUT
