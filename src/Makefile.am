include $(top_srcdir)/common.mk

AM_CXXFLAGS = $(warning_flags)

noinst_LIBRARIES = libzs.a
libzs_a_SOURCES = zs.hh\
		decl.hh\
		util.cc util.hh util.i.hh\
		token.cc token.hh token.i.hh\
		number.cc number.hh number.i.hh\
		lisp_ptr.cc lisp_ptr.hh lisp_ptr.i.hh\
		cons.cc cons.hh cons.i.hh\
		symbol.cc symbol.hh symbol.i.hh\
		reader.cc reader.hh\
		printer.cc printer.hh\
		eval.cc eval.hh\
		procedure.cc procedure.hh procedure.i.hh\
		port.cc port.hh\
		builtin_util.cc builtin_util.hh\
		builtin.cc builtin.hh builtin.i.hh\
		builtin_numeric.cc builtin_numeric.hh\
		builtin_boolean.cc builtin_boolean.hh\
		vm.cc vm.hh vm.i.hh
libzs_a_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)

noinst_PROGRAMS = zs
zs_SOURCES = zs.hh main.cc
zs_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)
zs_LDADD = libzs.a

check_LIBRARIES = libzs_test.a
libzs_test_a_SOURCES = $(libzs_a_SOURCES)
libzs_test_a_CXXFLAGS = $(test_flags) $(AM_CXXFLAGS)
