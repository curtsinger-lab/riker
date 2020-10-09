AC_DEFUN([MACRO],
[AC_REQUIRE([AC_CHECK_DECLS([sys_siglist],[],[],[#include <signal.h>
/* NetBSD declares sys_siglist in unistd.h.  */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
])
])
AC_CHECK_DECLS([_sys_siglist], [], [], [#include <signal.h>])
])

AC_INIT
MACRO
AC_OUTPUT
