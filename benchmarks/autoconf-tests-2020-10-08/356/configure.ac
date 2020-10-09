AC_INIT
AC_CONFIG_HEADERS(config.h:config.hin)
AC_STATE_SAVE(before)
AC_CHECK_DECLS([yes, no, myenum, mystruct, myfunc, mymacro1, mymacro2],,,
		 [[int yes = 1;
		   enum { myenum };
		   struct { int x[20]; } mystruct;
		   extern int myfunc();
		   #define mymacro1(arg) arg
		   #define mymacro2]])
  # The difference in space-before-open-paren is intentional.
  AC_CHECK_DECLS([basenam (char *), dirnam(char *),
		  [moreargs (char, short, int, long, void *, char [], float, double)]],,,
		 [[#ifdef __cplusplus
		   extern "C++" char *basenam (char *);
		   extern "C++" const char *basenam (const char *);
		   #else
		   extern char *basenam (const char *);
		   #endif
		   #ifdef __cplusplus
		   extern "C" {
		   #endif
		   extern int moreargs (char, short, int, long, void *,
					char [], float, double);
		   #ifdef __cplusplus
		   }
		   #endif
		   ]])
  AC_CHECK_DECL([declared (char *)],, [AS_EXIT([1])],
		[[#ifdef __cplusplus
		  extern "C++" char *declared (char *);
		  extern "C++" const char *declared (const char *);
		  #else
		  extern char *declared (const char *);
		  #endif
		]])
  AC_CHECK_DECL([undeclared (char *)], [AS_EXIT([1])],, [[]])

AC_OUTPUT
AC_STATE_SAVE(after)
