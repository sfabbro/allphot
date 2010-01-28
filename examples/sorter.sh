    # compute automatic id offset (max of id)
    if [[ ${idoff} == auto ]]; then
	daophot_sort 1 ${infile} ${infile}.tmp N > ${incat}.tmp.in
	daophot < ${incat}.tmp.in
	idoff=$(tail -n1 ${incat}.tmp | awk '{print $1}')
	(( idoff = ${idoff} + 1 ))
	rm -f ${incat}.tmp.in ${infile}.tmp
    fi
