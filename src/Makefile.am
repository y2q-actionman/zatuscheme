include $(top_srcdir)/common.mk

AM_CXXFLAGS = $(warning_flags)

noinst_LIBRARIES = libzs.a
libzs_a_SOURCES = zs.hh\
		token.cc token.hh

noinst_PROGRAMS = zs
zs_SOURCES = zs.hh main.cc
zs_LDADD = libzs.a

check_LIBRARIES = libzs.a
