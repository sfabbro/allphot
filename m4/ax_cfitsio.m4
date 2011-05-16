# -*- autoconf -*-
# 
# use as AX_CFITSIO([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
#

AC_DEFUN([AX_CFITSIO],[

ax_cfitsio_ok=no

PKG_CHECK_MODULES(CFITSIO, cfitsio, [ax_cfitsio_ok=yes], [ax_cfitsio_ok=no])

if test x"$ax_cfitsio_ok" = x"no"; then
   # if neither pkg-config file found, nor CFITSIO_LIBS defined
   if test x$CFITSIO_LIBS = x ; then
      CFITSIO_LIBS="-lcfitsio -lm"
      AC_ARG_WITH([cfitsio-libdir],
	 [  --with-cfitsio-libdir=DIR directory where the library was installed],
	 [CFITSIO_LIBS="-L$withval $CFITSIO_LIBS"], )
   fi
   ax_cfitsio_lib_ok=no
   LIBS_sav="$LIBS"
   LIBS="$LIBS $CFITSIO_LIBS"
   AC_CHECK_LIB([cfitsio], [ffopen], [ax_cfitsio_lib_ok=yes], [AC_MSG_WARN([  *** cfitsio library not found])])

   # if neither pkg-config file found, nor CFITSIO_CFLAGS defined
   if test x$CFITSIO_CFLAGS = x ; then
      CFITSIO_CFLAGS=""
      AC_ARG_WITH(cfitsio-includedir,
	 [  --with-cfitsio-includedir=DIR directory where the headers were installed],
	 [CFITSIO_CFLAGS="-I$withval"], )
   fi
   ax_cfitsio_hdr_ok=no
   CPPFLAGS_sav="$CPPFLAGS"
   CPPFLAGS="$CPPFLAGS $CFITSIO_CFLAGS"
   AC_CHECK_HEADER([fitsio.h],
      [ax_cfitsio_hdr_ok=yes],
      [AC_MSG_WARN([  *** cfitsio headers not found.])])

   if test x$ax_cfitsio_lib_ok = xyes -a x$ax_cfitsio_hdr_ok = xyes; then
      ax_cfitsio_ok=yes
   fi
   AC_SUBST(CFITSIO_CFLAGS)
   AC_SUBST(CFITSIO_LIBS)
   LIBS="$LIBS_sav"
   CPPFLAGS="$CPPFLAGS_sav"
fi


# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$ax_cfitsio_ok" = x"yes"; then
        ifelse([$1],,AC_DEFINE(HAVE_CFITSIO, [1], [Define if you have CFITSIO library.]),[$1])
        :
else
        ax_cfitsio_ok=no
        $2
fi
])
