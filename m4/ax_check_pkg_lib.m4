#
# SYNOPSIS
#
#   AX_CHECK_PKG_LIB(PKG, [HEADER], [SYMBOL-IN-LIBRARY],
#                    [ACTION-IF-FOUND], [ACTION-IF-NOT_FOUND])
#
# DESCRIPTION
#
#   Provides a generic test for a given library. It follows pkg-config
#   style, and adds the following user configuration options to specify
#   both pre-processing and linking:
#
#   	   --with-<pkg>-cflags="-I/path/to/include -I/another/path"
#   	   --with-<pkg>-libs="-L/path/to/libdir -lpkg -L/another/libdir"
#	   
#   Just like pkg-config, it will define <PKG>_CFLAGS and <PKG>_LIBS.
#   In order, it will check:
#      1. whether <PKG>_CFLAGS and <PKG>_LIBS were set as environment
#      	  variables
#      2. whether the user set them with the --with-<lib>-cflags/libs options
#      3. whether pkg-config recognizes the libraries and sets these variables
#      4. if none of the above, set the variable <PKG>_CFLAGS as "" and
#         <PKG>_LIBS="-l<pkg>"
#
#   Finally, the headers and library symbols are checked.
#      	  
#   Example:
#
#     AX_CHECK_PKG_LIB([cfitsio], [fitsio.h], [ffopen], [],
#                      [AC_MSG_ERROR([Cound not find a cfitsio library])])
#
# LICENSE
#
#   Copyright (c) 2012 Sebastien Fabbro <sebfabbro@gmail.com>
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation, either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

AC_DEFUN([AX_CHECK_PKG_LIB],[

AC_ARG_VAR(m4_toupper($1)[_CFLAGS],
	   [Preprocessing flags for ]$1[ headers])
AC_ARG_VAR(m4_toupper($1)[_LIBS],
	   [Linking flags for ]$1[ libraries])

# check if user provided --with-<pkg>-cflags
AC_ARG_WITH(m4_tolower($1)[-cflags],
	    AS_HELP_STRING([--with-m4_tolower($1)-cflags=CFLAGS],
		           [Preprocessing flags for $1 headers]),
	    m4_toupper($1)[_CFLAGS="$withval"])

# check if user provided --with-<pkg>-libs
AC_ARG_WITH(m4_tolower($1)[-libs],
	    AS_HELP_STRING([--with-m4_tolower($1)-libs=LIBS],
			   [Linking flags for $1 libraries]),
	    m4_toupper($1)[_LIBS="$withval"])

# check with pkg-config
AS_IF([ test x"$]m4_toupper($1)[_LIBS" = x ],
      [PKG_CHECK_MODULES(m4_toupper($1), 
      			 m4_tolower($1),
			 m4_toupper($1)[_PC=]m4_tolower($1))])

# define default <PKG>_LIBS if none was set so far
AS_IF([ test x"$]m4_toupper($1)[_LIBS" = x ],
      m4_toupper($1)[_LIBS="-l]m4_tolower($1)["])

# now check header and symbol validity
AS_IF([ test x"]$3[" = x ],
      [ax_cv_symbol=main],
      [ax_cv_symbol=]$3)

AC_CACHE_VAL(AS_TR_SH([ax_cv_has_]m4_tolower($1)),
	     [save_CPPFLAGS="$CPPFLAGS"
	      save_LDFLAGS="$LDFLAGS"
	      AS_IF([ test x"$]m4_toupper($1)[_CFLAGS" != x ],
	            [CPPFLAGS="$CPPFLAGS $]m4_toupper($1)[_CFLAGS"])
	      AS_IF([ test "x$]m4_toupper($1)[_LIBS" != x ],
	      	    [LDFLAGS="$LDFLAGS $]m4_toupper($1)[_LIBS"])
	      AC_CHECK_HEADER($2,
		[AC_CHECK_LIB(m4_tolower($1),
			      [$ax_cv_symbol],
			      [AS_TR_SH([ax_cv_has_]m4_tolower($1))=yes],
         		      [AS_TR_SH([ax_cv_has_]m4_tolower($1))=no])],
		[AS_TR_SH([ax_cv_has_]m4_tolower($1))=no])
	      CPPFLAGS="$save_CPPFLAGS"
	      LDFLAGS="$save_LDFLAGS"])

AC_SUBST(m4_toupper($1)[_LIBS])
AC_SUBST(m4_toupper($1)[_CFLAGS])
AC_SUBST(m4_toupper($1)[_PC])

AS_IF([ test x"$]AS_TR_SH([ax_cv_has_]m4_tolower($1))[" = xyes ],
      AC_DEFINE([HAVE_]m4_toupper($1), [1], [Define to 1 if ]$1[ is found])
    [$4],
    [$5])

])
