#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([zatuscheme], [alpha.1], [http://github.com/y2q-actionman/zatuscheme/issues])
AM_INIT_AUTOMAKE([foreign color-tests])
AC_CONFIG_SRCDIR([src/token.cc])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX([g++])
AC_PROG_CC
AC_PROG_RANLIB

## add C++0x flags
## TODO: check for compiler's C++0x support
CXXFLAGS="${CXXFLAGS} -std=c++0x"

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_CHECK_FUNCS([floor pow sqrt strcasecmp strerror])

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile])
AC_OUTPUT
