#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([FULL-PACKAGE-NAME], [VERSION], [BUG-REPORT-ADDRESS])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([src/token.cc])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
CFLAGS=" "
CXXFLAGS=" "
AC_PROG_CC_C99([gcc])
AC_PROG_CXX([g++])

## add C++0x flags and other warning options
## TODO: check for compiler's C++0x support
warnings="-Wall -Wextra -Wconversion -Wpointer-arith -Wcast-qual -Wcast-align -Wlogical-op -Wredundant-decls"
CFLAGS="${CFLAGS} ${warnings}"
CXXFLAGS="${CXXFLAGS} ${warnings} -std=c++0x"

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile])
AC_OUTPUT
