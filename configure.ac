AC_INIT([allphot],[0.8.2],[sfabbro@uvic.ca])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([bin/allphot.in])
AC_CONFIG_AUX_DIR([autoconf])

AM_INIT_AUTOMAKE([foreign])


# BASH may already be set in the shell, if the admin then changes the
# the /bin/sh symlink to a non-bash shell, all hell will break lose.
unset BASH
AC_PATH_PROGS(BASH, bash)
if test x$BASH = "x" ; then
    AC_MSG_ERROR([bash is required])
fi

AX_PROG_GNU_SED
if test x$SED = "x" ; then
    AC_MSG_ERROR([GNU sed is required])
fi

AC_PATH_PROG(READLINK, greadlink)
if test x$READLINK = "x" ; then
    AC_CHECK_PROG(READLINK, readlink -f ., yes, no, [$PATH])
    if test x$READLINK = "xyes" ; then
        AC_PATH_PROG(READLINK, readlink)
    fi

    CANONICALISE_TEST=`$READLINK -f . > /dev/null 2>&1`
    if ! test x$? = "x0" ; then
        unset READLINK
    fi

    if test x$READLINK = "x" ; then
        AC_PATH_PROG(REALPATH, realpath)
        if test x$REALPATH = "x" ; then
            AC_MSG_ERROR([Either GNU readlink or realpath is required])
        fi
    fi
fi
if test x$READLINK != "x"; then
    CANONICALISE="$READLINK -f"
else
    CANONICALISE="$REALPATH"
fi
AC_SUBST(CANONICALISE)

AC_PATH_PROGS(DAOPHOT, daophot)
if test x$DAOPHOT = "x" ; then
    AC_MSG_WARN([daophot command not found. Needed at runtime for "allphot daophot"])
fi

AC_PATH_PROGS(ALLSTAR, allstar)
if test x$ALLSTAR = "x" ; then
    AC_MSG_WARN([allstar command not found. Needed at runtime for "allphot allstar"])
fi

AC_PATH_PROGS(DAOMATCH, daomatch)
if test x$DAOMATCH = "x" ; then
    AC_MSG_WARN([daomatch command not found. Needed at runtime for "allphot daomatch"])
fi

AC_PATH_PROGS(DAOMASTER, daomaster)
if test x$DAOMASTER = "x" ; then
    AC_MSG_WARN([daomaster command not found. Needed at runtime for "allphot daomaster"])
fi

AC_PATH_PROGS(ALLFRAME, allframe)
if test x$ALLFRAME = "x" ; then
    AC_MSG_WARN([allframe command not found. Needed at runtime for "allphot allframe"])
fi

AC_PATH_PROGS(ALLFRAME, montage2)
if test x$ALLFRAME = "x" ; then
    AC_MSG_WARN([montage2 command not found. Needed at runtime for "allphot montage2"])
fi

AC_PATH_PROGS(BC, bc)
if test x$BC = "x" ; then
    AC_MSG_ERROR([bc calculator is required])
fi

AC_PROG_CC

AX_CHECK_PKG_LIB(
	[cfitsio],
	[fitsio.h],
	[ffopen],,
	[AC_MSG_ERROR([Could not find cfitsio library])])

AC_CHECK_LIB([m], [floor])

PACKAGE_UPNAME=`echo $PACKAGE_NAME | tr '[:lower:]' '[:upper:]'`
AC_SUBST(PACKAGE_UPNAME)

AC_CONFIG_FILES([Makefile 
		data/Makefile
		src/Makefile
		tests/Makefile
		bin/Makefile
		libs/Makefile
		modules/Makefile
		examples/Makefile])
AC_OUTPUT
