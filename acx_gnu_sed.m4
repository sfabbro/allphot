AC_DEFUN([ACX_PROG_GNU_SED],
[AC_MSG_CHECKING([for GNU sed])
AC_CACHE_VAL(acx_cv_path_SED,
[# Loop through the user's path and test for sed and gsed.
saved_IFS=$IFS ; IFS=:
for acx_dir in $PATH
do
    IFS=$saved_IFS
    if test -x "$acx_dir/sed" ; then
	if "$acx_dir/sed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null ; then
	    acx_path_sed="$acx_dir/sed"
	fi
    fi

    if test -z "$acx_path_sed" && test -x "$acx_dir/gsed" ; then
	if "$acx_dir/gsed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null ; then
	    acx_path_sed="$acx_dir/gsed"
	fi
    fi

    acx_cv_path_SED=$acx_path_sed
done
])
SED=$acx_cv_path_SED
AC_MSG_RESULT([$SED])
AC_SUBST(SED)
])
