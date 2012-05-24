include $(top_srcdir)/common.mk

AM_CXXFLAGS = $(warning_flags)

noinst_LIBRARIES = libzs.a
libzs_a_SOURCES = zs.hh\
		decl.hh\
		util.cc util.hh\
		token.cc token.hh token.i.hh\
		number.cc number.hh number.i.hh\
		lisp_ptr.cc lisp_ptr.hh lisp_ptr.i.hh\
		cons.cc cons.hh cons.i.hh\
		symbol.cc symbol.hh symbol.i.hh\
		symtable.cc symtable.hh\
		keyword.cc keyword.hh\
		reader.cc reader.hh\
		printer.cc printer.hh\
		eval.cc eval.hh\
		function.cc function.hh function.i.hh\
		builtin.cc builtin.hh\
		vm.cc vm.hh vm.i.hh

libzs_a_CXXFLAGS = $(release_flags) $(AM_CXXFLAGS)

noinst_PROGRAMS = zs
zs_SOURCES = zs.hh main.cc
zs_LDADD = libzs.a

check_LIBRARIES = libzs_test.a
libzs_test_a_SOURCES = $(libzs_a_SOURCES)
libzs_test_a_CXXFLAGS = $(test_flags) $(AM_CXXFLAGS)
