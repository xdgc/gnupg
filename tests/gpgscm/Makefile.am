# TinyScheme-based test driver.
#
# Copyright (C) 2016 g10 Code GmbH
#
# This file is part of GnuPG.
#
# GnuPG is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# GnuPG is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

EXTRA_DIST = \
	COPYING \
	init.scm

AM_CPPFLAGS = -I$(top_srcdir)/common
include $(top_srcdir)/am/cmacros.am

AM_CFLAGS =

bin_PROGRAMS = gpgscm

common_libs = ../$(libcommon)
commonpth_libs = ../$(libcommonpth)

gpgscm_CFLAGS = -imacros scheme-config.h \
	$(LIBGCRYPT_CFLAGS) $(LIBASSUAN_CFLAGS) $(GPG_ERROR_CFLAGS)
gpgscm_SOURCES = main.c scheme-config.h ffi.c ffi.h ffi-private.h \
	opdefines.h scheme.c scheme.h scheme-private.h
gpgscm_LDADD = $(LDADD) $(common_libs) \
	$(NETLIBS) $(LIBICONV) $(LIBREADLINE) \
	$(LIBGCRYPT_LIBS) $(GPG_ERROR_LIBS)

# Make sure that all libs are build before we use them.  This is
# important for things like make -j2.
$(PROGRAMS): $(common_libs)
