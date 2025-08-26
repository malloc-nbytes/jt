AC_INIT([jt], [1.0], [zdhdev@yahoo.com])
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADERS([src/config.h])
AM_INIT_AUTOMAKE([-Wall foreign])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# Set C standard to C23
AX_CHECK_COMPILE_FLAG([-std=c23], [CFLAGS="$CFLAGS -std=c23"], [AC_MSG_ERROR([Compiler does not support C23])])

# Checks for libraries.
AC_CHECK_LIB([forge], [main], [], [AC_MSG_ERROR([Could not find libforge])])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h stdio.h])

# Output files
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
