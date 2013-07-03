#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([zatuscheme], [alpha.1], [http://github.com/y2q-actionman/zatuscheme/issues])
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

## case selection
AC_ARG_ENABLE([case],
        [AS_HELP_STRING([--enable-case],
                [specifies reading case from upper,lower (default is lower)])],
        [],
        [enable_case=lower])

AS_IF([test "x$enable_case" = xupper],
            [AC_DEFINE([USE_CASE_UPPER], [1], [reading case is upper])],
      [test "x$enable_case" = xlower],
            [AC_DEFINE([USE_CASE_LOWER], [1], [reading case is lower])],
      AC_MSG_ERROR([cannot use specified reading case: $enable_case]))

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile])
AC_OUTPUT
