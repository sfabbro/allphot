AC_DEFUN([AP_PROG_GNU_SED],
[AC_MSG_CHECKING([for GNU sed])
AC_CACHE_VAL(es_cv_path_SED,
[# Loop through the user's path and test for sed and gsed.                      
saved_IFS=$IFS ; IFS=:
for es_dir in $PATH
do
    IFS=$saved_IFS
    if test -x "$es_dir/sed" ; then
        if "$es_dir/sed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null ; \
then
            es_path_sed="$es_dir/sed"
        fi
    fi

    if test -z "$es_path_sed" && test -x "$es_dir/gsed" ; then
        if "$es_dir/gsed" --version 2>&1 < /dev/null | grep 'GNU' > /dev/null ;\
 then
            es_path_sed="$es_dir/gsed"
        fi
    fi

    es_cv_path_SED=$es_path_sed
done
])
SED=$es_cv_path_SED
AC_MSG_RESULT([$SED])
AC_SUBST(SED)
])
