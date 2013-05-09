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
		enclose_test\
		quasiquote_test\
		builtin_char_test\
		builtin_cons_test\
		builtin_control_test\
		builtin_equal_test\
		builtin_numeric_test\
		builtin_port_test\
		builtin_char_ready_test\
		builtin_string_test\
		builtin_symbol_test\
		builtin_syntax_test\
		builtin_vector_test\
		delay_test\
		macro_test\
		extra_test\
		zs_test_main


TESTS = $(check_PROGRAMS) \
	r5rs_pitfall.scm \
	builtin_boolean_test.scm

TESTS_ENVIRONMENT = \
	export MALLOC_CHECK_=2; \
	ulimit -t 30; 

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = ./zs_test_main
