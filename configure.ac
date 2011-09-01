AC_INIT([allphot], [0.7.3], [sfabbro@uvic.ca])
AC_PREREQ(2.59)
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

dnl AC_PROG_SED is available only in autoconf 2.60 and later.
dnl With older autoconf, assume that 'sed' is ok.
ifdef([AC_PROG_SED], [AC_PROG_SED], [SED=sed AC_SUBST(SED)])

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
    AC_MSG_ERROR([daophot is required])
fi

AC_PATH_PROGS(ALLSTAR, allstar)
if test x$ALLSTAR = "x" ; then
    AC_MSG_ERROR([allstar is required])
fi

AC_PATH_PROGS(DAOMATCH, daomatch)
if test x$DAOMATCH = "x" ; then
    AC_MSG_ERROR([daomatch is required])
fi
AC_PATH_PROGS(DAOMASTER, daomaster)
if test x$DAOMASTER = "x" ; then
    AC_MSG_ERROR([daomaster is required])
fi
AC_PATH_PROGS(ALLFRAME, allframe)
if test x$ALLFRAME = "x" ; then
    AC_MSG_ERROR([allframe is required])
fi

AC_PATH_PROGS(BC, bc)
if test x$BC = "x" ; then
    AC_MSG_ERROR([bc calculator is required])
fi

AC_PROG_CC
AX_CFITSIO

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
