#!/bin/bash

# macho_dict [<name>]: produce a basic dict file for MACHO for ALLPHOT
macho_dict() {
    local dict=${1:-macho}.dict
    cat > ${dict} <<EOF
GA=fits(CS-GAIN)
RE=fits(RDNOISE)
HI=fits(SATURATE)
FW=fits(SEEING)
AN=4
EOF
    echo ${dict}
}

# macho_fetch <file id>: fetch a MEF FITS file ID from CADC
# return the name of the downloaded file
macho_fetch() {
    local fileid="${1%%[*}"
    local cutout="${1/${fileid}}"    
    fileid=${fileid%%.fits*}
    local input="http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/getData?archive=MACHO&asf=true&file_id=${fileid}&dua=true"
    local suf=
    if [[ -n ${cutout} ]]; then
	input="${input}&cutout=${cutout}"
	suf=$(printf "%02d" $(echo "${cutout:1}" | cut -f1 -d ']'))
    fi
    wget -nv \
	--output-document="${fileid}${suf}.fits.fz" \
	"${input}" &> /dev/null
}

# fits_cut <fits>: uncompress and keep datasec only
# return the new name
fits_cut() {
    imcopy "${1}$(fits_key DATASEC ${1})" ${1%%.*}.fits
    rm -f ${1}
    echo ${1%%.*}.fits
}

# fits_split <mef fits>: split a MEF FITS files and keep datasec only
fits_split() {
    local nhdu=$(fitshead ${1} | grep ^END$ | wc -l)
    [[ ${nhdu} == 1 ]] && fits_cut ${1} && return
    local hdu
    for (( hdu=1; hdu < ${nhdu}; ++hdu )); do
        imcopy \
	    "${1}[${hdu}]$(fitskey -n -p DATASEC ${1}[${hdu}])" \
	    "${1%%.*}0${hdu}.fits"
	echo ${1%%.*}0${hdu}.fits
    done
    rm -f ${1}
}

# allstar_cleanup <fits> : make a tar ball from DAOPHOT/ALLSTAR important files
allstar_cleanup() {
    local im=${1%.*} outdir=allstar-${im}
    mkdir ${outdir}
    mv -f ${im}.{opt,als,psf,log} ${outdir}
    tar cfj output-${im}.tar.bz2 ${outdir}
    rm -rf process_* ${im}* *.dict ${outdir}
}

# macho_process <file id>: fetch MEF, process each CCD with DAOPHOT/ALLSTAR
macho_process() {
    echo " >>> Fetching ${1}"
    local im=$(macho_fetch ${1})
    [[ ! -e ${im} ]] && echo "File ${im} not valid" && return
    echo " >>> Splitting ${im}"
    local ccd
    for ccd in $(fits_split ${im}); do
	echo " >>> Running DAOPHOT/ALLSTAR for ${ccd}"
	iter_allstar.bash --dict="$(macho_dict)" ${ccd} &> ${ccd%.*}.log
	allstar_cleanup ${ccd}
    done
}

[[ $# -lt 1 ]] && echo "$(basename $0) <file id>...<file id>" && exit
for fileid in $@; do
    macho_process ${fileid}
done
