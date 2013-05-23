include $(top_srcdir)/common.mk

AM_CPPFLAGS = -I../src
AM_CXXFLAGS = $(warning_flags) $(test_flags)

AM_DEFAULT_SOURCE_EXT = .cc

test_lib = test_util.o describe.o

LDADD = $(test_lib) \
	../src/zs.hh \
	../src/libzs_test.a

check_PROGRAMS = token_test\
		parse_number_test\
		printer_test\
		function_arg_test\
		reader_test\
		eval_test\
		zs_test_main


TESTS = $(check_PROGRAMS) \
	macro_test.scm \
	delay_test.scm \
	enclose_test.scm \
	quasiquote_test.scm \
	builtin_boolean_test.scm \
	builtin_char_test.scm \
	builtin_char_ready_test.scm \
	builtin_cons_test.scm \
	builtin_control_test.scm \
	builtin_equal_test.scm \
	builtin_numeric_test.scm \
	builtin_port_test.scm \
	builtin_string_test.scm \
	builtin_symbol_test.scm \
	builtin_syntax_test.scm \
	builtin_vector_test.scm \
	extra_test_exit0.scm \
	extra_test_exit1.scm \
	extra_test_error.scm \
	r5rs_pitfall.scm

XFAIL_TESTS = extra_test_exit1.scm

TESTS_ENVIRONMENT = \
	export MALLOC_CHECK_=2; \
	ulimit -t 30; 

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = ./zs_test_main
