AM_CPPFLAGS = @CFITSIO_CFLAGS@
CSRC =  executils.h \
	fitskey.h fitskey.c \
	fitsutils.h fitsutils.c \
	stringutils.h stringutils.c

LDADD = @CFITSIO_LIBS@

bin_PROGRAMS = fitshead fitskey
fitshead_SOURCES = $(CSRC) main_fitshead.c
fitskey_SOURCES	 = $(CSRC) main_fitskey.c
