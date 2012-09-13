AC_DEFUN([AX_PROG_GNU_SED],
[AC_MSG_CHECKING([for GNU sed])

AC_CACHE_VAL(ax_cv_path_SED,
[# Loop through the user's path and test for sed and gsed.
saved_IFS=$IFS ; IFS=:
for ax_dir in $PATH
do
    IFS=$saved_IFS
    if test -x "$ax_dir/sed" ; then
	if "$ax_dir/sed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null ; then
	    ax_path_sed="$ax_dir/sed"
	fi
    fi

    if test -z "$ax_path_sed" && test -x "$ax_dir/gsed" ; then
	if "$ax_dir/gsed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null ; then
	    ax_path_sed="$ax_dir/gsed"
	fi
    fi

    ax_cv_path_SED=$ax_path_sed
done
])
SED=$ax_cv_path_SED
AC_MSG_RESULT([$SED])
AC_SUBST([SED])
])
