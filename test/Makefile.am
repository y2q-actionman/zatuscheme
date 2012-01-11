include $(top_srcdir)/common.mk

AM_CPPFLAGS = -I../src
AM_CXXFLAGS = $(warning_flags) $(test_flags)

AM_DEFAULT_SOURCE_EXT = .cc

LDADD = ../src/libzs_test.a
check_PROGRAMS =

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = $(test_envs)
