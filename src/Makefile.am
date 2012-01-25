include $(top_srcdir)/common.mk

AM_CXXFLAGS = $(warning_flags)

noinst_LIBRARIES = libzs.a
libzs_a_SOURCES = zs.hh\
		token.cc token.hh token.i.hh\
		number.cc number.hh number.i.hh\
		lisp_ptr.cc lisp_ptr.hh lisp_ptr.i.hh\
		cons.cc cons.hh\
		keyword.cc keyword.hh

noinst_PROGRAMS = zs
zs_SOURCES = zs.hh main.cc
zs_LDADD = libzs.a

check_LIBRARIES = libzs_test.a
libzs_test_a_SOURCES = $(libzs_a_SOURCES)
libzs_test_a_CXXFLAGS = $(test_flags) $(AM_CXXFLAGS)
