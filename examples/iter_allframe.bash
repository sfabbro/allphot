#!/bin/bash

match_master() {
    local cur=${1%.*}
    allphot daomatch --ref=${FIELD}.mag --out=${cur}.mch $1
    allphot daomaster \
	--frames=1,0.5,1 \
	--dof=20 \
	--mag=${cur}.mag \
	--raw=no \
	--cor=no \
	--tfr=no \
	--no-coo \
	--no-mtr \
	${cur}.mch
    mv ${cur}.mag ${FIELD}.mag
    if ! grep -q ${FIELD}.mag ${FIELD}.mch 2> /dev/null; then
	echo > ${FIELD}.mch "'${FIELD}.mag'     0.0000    0.0000 1.000000000 0.000000000 0.000000000 1.000000000    0.000  0.0000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000"
    fi
    tail -n1 ${cur}.mch >> ${FIELD}.mch
    rm -f ${cur}.mch
    allphot allframe \
	--option GE=20 \
	--mag=${FIELD}.mag
    mv ${FIELD}.{nmg,mag}
}

detect_residuals() {
    local ref=${1%.*}
    #TOFIX: determine limits from ${ref}.mch or ${ref}.mag?
    allphot montage2 \
	--suffix=j.fits \
	--frames=${nf},0.5 \
	--limits="[${x0}:${x1},${y0},${y1}]" \
	--fits=${FIELD}j.fits \
	${FIELD}.mch
    allphot daophot find ${FIELD}j.fits
    allphot daophot phot ${FIELD}j.fits
    #TOFIX: offset dx dy to make sure ${FIELD}j.ap and ${FIELD}.mag are in same referential
    allphot append --out=${FIELD}j.app ${FIELD}j.ap ${FIELD}.mag
    allphot sort --renum  --index=2 --out=${FIELD}.mag ${FIELD}.app
}


FIELD=ref

usage() {
    echo "Usage: $(basename ${0}) [--out=BASENAME] FILE..."
    exit
}

[[ $# -lt 1 ]] && usage

while [[ $# -gt 0 ]]; do
    case "${1}" in
	--help) usage ;;
	--out=*) FIELD=${1##*=} ;;
	--*) echo "Unrecognized option: ${1}" 1>&2 ; exit ;;
        *) break ;;
    esac
    shift
done

# start with first catalogue
cp $1 ${FIELD}.mag

echo ${FIELD}.mch

# match each catalogue to improve at each iteration
for f in $@; do
    match_master ${f}    
done

# Run allframe 
master_allframe ${FIELD}.mch

# Stack residuals and detect leftover
detect_residuals ${FIELD}.mch

# Re-run allframe with the combined list
master_allframe ${FIELD}.mch
