include $(top_srcdir)/common.mk

AM_CPPFLAGS = -I../src
AM_CXXFLAGS = $(warning_flags) $(test_flags)

AM_DEFAULT_SOURCE_EXT = .cc

LDADD = test_util.o\
	describe.o\
	../src/libzs_test.a

check_PROGRAMS = token_test\
		parse_number_test\
		printer_test\
		reader_test\
		eval_test\
		function_arg_test\
		enclose_test\
		builtin_test\
		builtin_numeric_test\
		quasiquote_test

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = $(test_envs)
