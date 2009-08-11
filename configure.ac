AC_PREREQ(2.61)
AC_INIT([allphot], [4.0.1], [pbs@nrc.dao.ca])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/daophot.f])
AC_CONFIG_AUX_DIR([config])
AC_CANONICAL_HOST
AM_INIT_AUTOMAKE

AC_PROG_F77

dnl automatically add -lm to all linking steps
AC_CHECK_LIB(m,cos)

sinclude(acx_cfitsio.m4)
ACX_CFITSIO(,
 	[AC_MSG_ERROR([cfitsio library could not be detected
*** Please install cfitsio or use the options
*** --with-cfitsio, or --with-cfitsio-includedir and --with-cfitsio-libdir.])])

AC_CONFIG_FILES([Makefile 
		src/Makefile
		doc/Makefile
		data/Makefile
		tests/Makefile
		scripts/Makefile])
AC_OUTPUT
