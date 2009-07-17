#!/bin/sh

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

daophot_make_opt() {
    local daofile=$(stripfilename ${1})
    local optfile="daophot.${daofile}.opt"
    local phofile="photo.${daofile}.opt"
    [ -e ${optfile} ] && [ -e ${phofile} ] && return 0

    local gain=$(fitskey GAIN ${1})    
    local rdnoise=$(fitskey RDNOISE ${1})
    local satur=$(fitskey SATURATE ${1})
    [ -z ${gain} ] && gain=1
    [ -z ${rdnoise} ] && rdnoise=0
    [ -z ${satur} ] && satur=65535
    cat <<-EOF > ${optfile}
	LO=7
	TH=3
	WA=-2
	VA=0
	AN=1
	RE=${rdnoise}
	GA=${gain}
	HI=${satur}
EOF
    ln -sfn ${optfile} daophot.opt
    cat  <<-EOF > ${phofile}
	A1=4
	A2=8
	A3=12
	IS=13
	OS=28
EOF
}

daophot_opt_update() {
    local image="$(stripfilename ${1})"
    local fwhm=$(get_fwhm ${1})
    cat <<-EOF >> "daophot.${image}.opt"
	VA=2
	AN=-6
	FW=${fwhm}
	FI=$(echo "${fwhm} * 1.4" | bc)
	PS=$(echo "${fwhm} * 3." | bc)
EOF
    cat <<-EOF > "photo.${image}.opt"
	A1=${fwhm}
	A2=$(echo "${fwhm} * 1.5" | bc)
	IS=$(echo "${fwhm} * 3" | bc)
	OS=$(echo "${fwhm} * 3 + 15" | bc)
EOF
}

### daophot commands ###
daophot_opt() {
    cat <<-EOF >> ${DAO_PROCESS}
	OPTION
	${1}

EOF
}

daophot_attach() {
    echo "ATTACH ${1}" >> ${DAO_PROCESS}
}

daophot_find() {
    mv -fu ${1} ${1}.old
    cat <<-EOF >> ${DAO_PROCESS}
	FIND
	1,1
	${1}
	y

EOF
}

daophot_phot() {
    mv -fu ${2} ${2}.old
    cat <<-EOF >> ${DAO_PROCESS}
	PHOT
	photo.${1%.*}.opt

EOF
    if [ -f ${1%.*}.psf ] && [ -f ${1%.*}.als ]; then
    cat <<-EOF >> ${DAO_PROCESS}
	${1}
	${1}
EOF
    fi
    cat <<-EOF >> ${DAO_PROCESS}
	${1}
	${2}
EOF
}

daophot_pick() {
    mv -fu ${2} ${2}.old
    local nstars=150
    local magfaint=15
    if [ -e ${1} ]; then
	magfaint=$(tail -n +4 ${1%.}.als | sort -n -k 4 | head -n ${nstars} | tail -n 1 | awk '{print $4}')
    fi
    cat <<-EOF >> ${DAO_PROCESS}
	PICK
	${1}
	${nstars} ${magfaint}
	${2}
EOF
}

daophot_psf() {
    mv -fu ${3} ${3}.old
    mv -fu ${3%.*}.nei ${3%.*}.nei.old
    # 2 rounds: normal then with neighbors subtracted
    cat <<-EOF >> ${DAO_PROCESS}
	PSF
	${1}
	${2}
	${3}
	PSF
	${3%.*}.nei
	${2}
	${3}

EOF
}

allstar_standard() {
    daoprocess_init ${1}
    local image="$(stripfilename ${1})"
    local fwhm=$(get_fwhm ${1})
    cat  <<-EOF > allstar.${image}.opt
	RE=1
	WA=1
	IS=2
	FI=$(echo "${fwhm} * 1.4" | bc)
	OS=$(echo "${fwhm} * 3" | bc)
EOF
    ln -sfn allstar.${image}.opt allstar.opt
    mv -fu ${image}.als  ${image}.als.old
    cat <<-EOF > ${DAO_PROCESS}

	${image}
	${image}.psf
	${image}.ap
	${image}.als
	${image}s
EOF
    daoprocess_do allstar
}

### recipes ###
daophot_standard() {
    daoprocess_init ${1}
    local image="$(stripfilename ${1})"
    daophot_make_opt ${1}
    daophot_opt daophot.${image}.opt
    daophot_attach ${image}
    daophot_find ${image}.coo
    daophot_phot ${image}.coo ${image}.ap
    daophot_pick ${image}.ap ${image}.lst
    daophot_psf ${image}.ap ${image}.lst ${image}.psf
    daoprocess_do daophot
}

daophot_redo_psf() {
    daoprocess_init ${1}
    local image="$(stripfilename ${1})"
    daophot_opt_update ${image}
    daophot_attach ${image}
    daophot_pick ${image}.${2} ${image}.lst
    daophot_psf ${image}.${2} ${image}.lst ${image}.psf
    daoprocess_do daophot
}

daophot_redo_phot() {
    daoprocess_init ${1}
    local image="$(stripfilename ${1})"
    daophot_opt_update ${image}
    daophot_attach ${image}
    daophot_phot ${image}.${2} ${image}.ap
    daoprocess_do daophot
}

allstar_iterate() {
    daophot_standard ${1}
    allstar_standard ${1}
    daophot_redo_psf ${1} als
    allstar_standard ${1}
    daophot_redo_phot ${1} als
}

#### main program ####
#/home/cadc/kreugerj/work/develop/macho/src/python/ccd with cutouts (headers)

# No arguments 
if [ $# -eq 0 ]; then
    echo $"Usage: $(basename $0) <FITS files>
Run DAOPHOT/ALLSTAR/ALLFRAME unattended for a bunch of FITS images.
"
    exit 0
fi

for fitsfile in $*; do
  echo "====== Processing ${fitsfile} ======="
  date
  allstar_iterate ${fitsfile} &> ${fitsfile}.log
done
