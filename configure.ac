#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([zatuscheme], [beta], [http://github.com/y2q-actionman/zatuscheme/issues])
AM_INIT_AUTOMAKE([foreign parallel-tests])
AC_CONFIG_SRCDIR([src/token.cc])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_LANG([C++])
AC_PROG_CXX
AX_CXX_COMPILE_STDCXX_11([noext],[mandatory])

AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([strcasecmp])

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile])
AC_OUTPUT
