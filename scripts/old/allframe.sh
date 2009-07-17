#!/bin/sh
#
# Shell script to run daophot as automatically as possible
# needs cfitsio (should be installed if daophot anyway)
#
stripfilename() {
    local fitsname="$(basename ${1})"
    echo ${fitsname%.*}
}

fitskey() {
    echo $(listhead ${2} 2> /dev/null | grep ^${1}.*= | cut -d '=' -f 2 | awk '{print $1}')
}


get_fwhm() {
    local fwhm=$(head -n 2 ${1%.*}.psf 2> /dev/null | tail -n 1 | awk '{print $1*2.35}')
    [ -z ${fwhm} ] && fwhm=4
    echo ${fwhm}
}

daoprocess_init() {
    if [ ! -f ${1} ]; then
	echo " Error: file ${1} not found."
	return 1
    fi
    local fitsdir=$(dirname ${1})
    export DAO_PROCESS="${fitsdir}/.$(stripfilename ${1}).daoprocess"
    touch ${DAO_PROCESS}
    pushd ${fitsdir} &> /dev/null
    return 0
}

daoprocess_do() {
    local prog=$(type -p ${1})
    if [ ! -x ${prog} ]; then
	echo "*** Error: program ${1} not found"
        return 1
    fi
    ${prog} < ${DAO_PROCESS}
    mv -fu ${DAO_PROCESS} ${DAO_PROCESS}.old
    unset DAO_PROCESS
    popd &> /dev/null
    return 0
}

# daomatch_standard <reference list> <mch file> <list of catalogs files>
daomatch_standard() {
    daoprocess_init ${1}
    mv -fu ${2} ${2}.old
    # Master input file
    echo ${1} > ${DAO_PROCESS}
    # Output file name 
    echo ${2} >> ${DAO_PROCESS}
    # Next input files
    cat ${3} >> ${DAO_PROCESS}
    echo >> ${DAO_PROCESS}
    echo y  >> ${DAO_PROCESS}
    daoprocess_do daomatch
}

# daomaster_standard <field name>
daomaster_standard() {
    local field=${1%.*}
    local niter=10
    local minrad=2
    local maxrad=5
    local dof=12
    local maxdm=0.2
    local minstars=5
    local minfrac=0.2
    local nframes=$(wc -l ${1} | cut -d ' ' -f 1)
    local minframes=$(wc -l ${1} | awk '{print int($1*0.1)}')
    [ ${minframes} -lt 2 ] && minframes=2
    daoprocess_init ${1}
    mv -uf ${1} ${field}.mch
    rm -f ${field}.{mag,cor,raw,tfr}
    awk '{print "rm -f "substr($1,2,match($1,/\./)-2)".{coo,mtr}"}' ${field}.mch | sh
    {
	echo ${field}.mch
	echo "${minstars}, ${minfrac}, ${minframes}"
	echo "${maxdm}"
	echo "${dof}"
	echo "${maxrad}"
	for i in $(seq ${nframes}); do echo ; done
	# matchup radii iterations
	for i in $(seq ${niter}); do
	    echo ${minrad}
	done
	echo 0
	#  Assign new star IDs? 
	echo y
	#  A file with mean magnitudes and scatter? 
	echo y
	echo ${field}.mag
	# A file with corrected magnitudes and errors?
	echo y
        echo ${field}.cor
        # A file with raw magnitudes and errors?
	echo y
	echo ${field}.raw
        # A file with the new transformations?
	echo y
        echo ${field}.mch
	# default overwrite
	echo
	# A file with the transfer table?
	echo y
	echo ${field}.tfr
        # Individual .COO files?
	echo y
	# Simply transfer star IDs?
	echo y
    } > ${DAO_PROCESS}
    daoprocess_do daomaster
}

# allframe_standard
allframe_standard() {
    daoprocess_init ${1}
    cat <<-EOF > allframe.opt
	CE=6.00
	CR=2.50
	GE=20
	WA=2
	MI=5
	MA=200
	PE=0.75
	PR=5.00
	IS=2.50
	OS=25.00
EOF
    echo > ${DAO_PROCESS}
    echo ${1} >> ${DAO_PROCESS}
    echo ${2} >> ${DAO_PROCESS}
    daoprocess_do allframe
}

#### main program ####

# No arguments 
if [ $# -eq 0 ]; then
    echo $"Usage: $(basename $0) <catalogue files>
Run DAOMATCH/DAOMASTER/ALLFRAME unattended for a bunch of starlists.
"
    exit 0
fi

ls -1 $* | tail --lines=+2 > daomatch.list
daomatch_standard $1 macho.mch daomatch.list
daomaster_standard macho.mch
allframe_standard macho.mch macho.mag
