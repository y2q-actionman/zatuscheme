include $(top_srcdir)/common.mk

AM_CXXFLAGS = $(warning_flags)

noinst_LIBRARIES = libzs.a
libzs_a_SOURCES = zs.hh\
		decl.hh\
		util.cc util.hh util.i.hh\
		zs_error.cc zs_error.hh\
		token.cc token.hh token.i.hh\
		number.cc number.hh number.i.hh\
		lisp_ptr.cc lisp_ptr.hh lisp_ptr.i.hh\
		cons.cc cons.hh cons.i.hh\
		cons_util.cc cons_util.hh cons_util.i.hh\
		symbol.cc symbol.hh\
		reader.cc reader.hh\
		printer.cc printer.hh\
		eval.cc eval.hh\
		procedure.cc procedure.hh procedure.i.hh\
		env.cc env.hh\
		delay.cc delay.hh\
		s_closure.cc s_closure.hh\
		s_rules.cc s_rules.hh\
		equality.cc equality.hh equality.i.hh\
		builtin_util.cc builtin_util.hh\
		builtin.cc builtin.hh builtin.i.hh\
		builtin_boolean.cc builtin_boolean.hh builtin_boolean.defs.hh\
		builtin_char.cc builtin_char.hh builtin_char.defs.hh\
		builtin_cons.cc builtin_cons.hh builtin_cons.defs.hh builtin_cons.strs.hh\
		builtin_equal.cc builtin_equal.hh builtin_equal.defs.hh\
		builtin_extra.cc builtin_extra.hh builtin_extra.defs.hh builtin_extra.strs.hh\
		builtin_numeric.cc builtin_numeric.hh builtin_numeric.defs.hh\
		builtin_port.cc builtin_port.hh builtin_port.defs.hh builtin_port.strs.hh\
		builtin_procedure.cc builtin_procedure.hh builtin_procedure.defs.hh builtin_procedure.strs.hh\
		builtin_string.cc builtin_string.hh builtin_string.defs.hh\
		builtin_symbol.cc builtin_symbol.hh builtin_symbol.defs.hh\
		builtin_syntax.cc builtin_syntax.hh builtin_syntax.defs.hh\
		builtin_vector.cc builtin_vector.hh builtin_vector.defs.hh\
		vm.cc vm.hh\
		zs.cc zs.hh
libzs_a_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)

bin_PROGRAMS = zatuscheme
zatuscheme_SOURCES = zs.hh main.cc
zatuscheme_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)
zatuscheme_LDADD = libzs.a

check_LIBRARIES = libzs_test.a
libzs_test_a_SOURCES = $(libzs_a_SOURCES)
libzs_test_a_CXXFLAGS = $(test_flags) $(AM_CXXFLAGS)
