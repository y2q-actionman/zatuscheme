include $(top_srcdir)/common.mk

AM_CXXFLAGS = $(warning_flags)

noinst_LIBRARIES = libzs.a
libzs_a_SOURCES = \
		builtin.cc builtin.hh builtin.defs.hh \
		builtin_boolean.cc builtin_boolean.hh \
		builtin_boolean.defs.hh \
		builtin_boolean.scm \
		builtin_char.cc builtin_char.hh \
		builtin_char.defs.hh \
		builtin_char.scm \
		builtin_cons.cc builtin_cons.hh \
		builtin_cons.defs.hh \
		builtin_cons.scm \
		builtin_equal.cc builtin_equal.hh \
		builtin_equal.defs.hh \
		builtin_extra.cc builtin_extra.hh \
		builtin_extra.defs.hh \
		builtin_extra.scm \
		builtin_numeric.cc builtin_numeric.hh \
		builtin_numeric.defs.hh \
		builtin_numeric.scm \
		builtin_port.cc builtin_port.hh \
		builtin_port.defs.hh \
		builtin_port.scm \
		builtin_procedure.cc builtin_procedure.hh \
		builtin_procedure.defs.hh \
		builtin_procedure.scm \
		builtin_srfi.cc builtin_srfi.hh \
		builtin_srfi.defs.hh \
		builtin_srfi.scm \
		builtin_string.cc builtin_string.hh \
		builtin_string.defs.hh \
		builtin_string.scm \
		builtin_symbol.cc builtin_symbol.hh \
		builtin_symbol.defs.hh \
		builtin_syntax.cc builtin_syntax.hh \
		builtin_syntax.defs.hh \
		builtin_syntax.scm \
		builtin_vector.cc builtin_vector.hh \
		builtin_vector.defs.hh \
		builtin_vector.scm \
		cons.cc cons.hh cons.i.hh \
		cons_util.cc cons_util.hh cons_util.i.hh \
		decl.hh decl.cc decl.i.hh \
		env.cc env.hh \
		equality.cc equality.hh equality.i.hh \
		eval.cc eval.hh \
		lisp_ptr.cc lisp_ptr.hh lisp_ptr.i.hh \
		printer.cc printer.hh \
		procedure.cc procedure.hh \
		rational.cc rational.hh rational.i.hh \
		reader.cc reader.hh \
		s_closure.cc s_closure.hh \
		s_rules.cc s_rules.hh \
		symbol.cc symbol.hh \
		token.cc token.hh token.i.hh \
		util.cc util.hh util.i.hh \
		vm.cc vm.hh \
		zs.cc zs.hh \
		zs_error.cc zs_error.hh \
		zs_memory.cc zs_memory.hh zs_memory.i.hh
libzs_a_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)

bin_PROGRAMS = zatuscheme
zatuscheme_SOURCES = zs.hh main.cc
zatuscheme_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)
zatuscheme_LDADD = libzs.a

check_LIBRARIES = libzs_test.a
libzs_test_a_SOURCES = $(libzs_a_SOURCES)
libzs_test_a_CXXFLAGS = $(test_flags) $(AM_CXXFLAGS)
