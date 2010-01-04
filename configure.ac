AC_PREREQ(2.61)
AC_INIT([allphot], [0.1], [sfabbro@uvic.ca])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_AUX_DIR([config])
AC_CANONICAL_HOST
AM_INIT_AUTOMAKE

# BASH may already be set in the shell, if the admin then changes the
# the /bin/sh symlink to a non-bash shell, all hell will break lose.
# thanks to James Rowe for the heads up.
unset BASH
AC_PATH_PROGS(BASH, bash)
if test x$BASH = "x" ; then
    AC_MSG_ERROR([bash is required])
fi

sinclude(acx_gnu_sed.m4)
ACX_PROG_GNU_SED
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
    AC_MSG_ERROR([daophot is required])
fi
AC_PATH_PROGS(ALLFRAME, allframe)
if test x$ALLFRAME = "x" ; then
    AC_MSG_ERROR([allframe is required])
fi

AC_CONFIG_FILES([Makefile 
		doc/Makefile
		data/Makefile
		tests/Makefile
		scripts/bin/Makefile
		scripts/libs/Makefile
		scripts/modules/Makefile
		scripts/Makefile])
AC_OUTPUT
