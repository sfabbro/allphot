# -*- autoconf -*-
# 
# use as ACX_CFITSIO([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
#

AC_DEFUN([ACX_CFITSIO],[

acx_cfitsio_ok=no

PKG_CHECK_MODULES([CFITSIO], [cfitsio $1], acx_cfitsio_ok=yes,[
    if test x$CFITSIO_LIBS = x ; then
       CFITSIO_LIBS="-lcfitsio -lm"
    fi

    AC_ARG_WITH(cfitsio,
	[  --with-cfitsio=DIR        directory where cfitsio source was compiled],
    	[CFITSIO_CFLAGS="-I$withval"
	 CFITSIO_LIBS="-L$withval $CFITSIO_LIBS"],
    )

    AC_ARG_WITH(cfitsio-includedir,
	[  --with-cfitsio-includedir=DIR directory where the headers were installed],
	[CFITSIO_CFLAGS=-I$withval],
    )

    AC_ARG_WITH(cfitsio-libdir,
	[  --with-cfitsio-libdir=DIR directory where the library was installed],
	[CFITSIO_LIBS=-L$withval $CFITSIO_LIBS],
    )

    CFLAGS_sav="$CFLAGS"
    CFLAGS="$CFLAGS $CFITSIO_CFLAGS"

    AC_CHECK_HEADER(fitsio.h, ,
	[AC_MSG_WARN([  *** Header file fitsio.h not found.])])

    LIBS_sav="$LIBS"
    LIBS="$LIBS $CFITSIO_LIBS"

    AC_CHECK_LIB(cfitsio, ffopen, acx_cfitsio_ok=yes,
    	[AC_MSG_WARN([ *** Library libcfitsio not linked properly])])

   AC_SUBST(CFITSIO_CFLAGS)
   AC_SUBST(CFITSIO_LIBS)

   LIBS="$LIBS_sav"
   CFLAGS="$CFLAGS_sav"
])

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_cfitsio_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_CFITSIO,1,[Define if you have CFITSIO library.]),[$1])
        :
else
        acx_cfitsio_ok=no
        $2
fi

])
