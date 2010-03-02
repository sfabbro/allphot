#!/bin/bash

make_dict() {
    local dict=${FIELD}.dict
    cat > ${dict} <<EOF
GA=fits(CS-GAIN)
RE=fits(RDNOISE)
HI=fits(SATURATE)
FW=fits(SEEING)
AN=4
EOF
    echo ${dict}
}

# cadc_fetch <file id>
# return the name of the downloaded file
# specify CADCSET for another set than MACHO
cadc_fetch() {
    local input="http://www.cadc.hia.nrc.gc.ca/getData?archive=${CADCSET:-MACHO}&asf=true&file_id=${1/[/&cutout=[}"
    local output="${1}.fits.fz" 
    [[ ${1: -1} == \] ]] && n=${1##*[} \
	output=$(printf "${1%[*}%02d.fits.fz" ${n%*]})
    wget -nv \
	--http-user=${CADCUSER:-sfabbro} \
	--http-password=${CADCPSSWD:-snlambda98} \
	-O ${output} \
	"${input}" &> /dev/null
    echo ${output}
}

fits_key() {
    listhead ${1} 2> /dev/null \
	| grep "^DATASEC.*=" \
	| cut -d '=' -f 2 \
	| awk '{print $1}' \
	| sed -e "s:'::g"
}

# fits_cut <fits>: uncompress and keep datasec only
fits_cut() {
    imcopy "${1}$(fits_key DATASEC ${1})" ${1%%.*}.fits
    rm -f ${1}
    echo ${1%%.*}.fits
}

fits_split() {
    local nhdu=$(listhead ${1} | grep ^END$ | wc -l)
    [[ ${nhdu} == 1 ]] && fits_cut ${1} && return
    local hdu
    for (( hdu=1; hdu < ${nhdu}; ++hdu )); do
        imcopy \
	    "${1}[${hdu}]$(fits_key DATASEC ${1}[${hdu}])" \
	    "${1%%.*}0${hdu}.fits"
	echo ${1%%.*}0${hdu}.fits
    done
    rm -f ${1}
}

clean_process() {
    local im=${1%.*}
    mkdir output
    mv -f ${im}.{opt,als,psf,log} output/
    tar cfj output-${im}.tar.bz2 output/*
    rm -rf process_* ${im}* *.dict output/
}

fileid_process() {
    echo " >>> Fetching ${1}"
    local im=$(cadc_fetch ${1})
    [[ ! -e ${im} ]] && echo "File ${im} not valid" && return
    echo " >>> Splitting ${im}"
    for ccd in $(fits_split ${im}); do
	echo " >>> Produce catalog and PSF for ${ccd}"
	allstar2.sh --dict="$(make_dict)" ${ccd} &> ${ccd%.*}.log
	#make_psf_als ${ccd} &> ${ccd%.*}.log
	echo " >>> Cleaning up ${ccd}"
	clean_process ${ccd}
    done
}

[[ $# -lt 1 ]] && echo "$(basename $0) <file id>...<file id>" && exit
for i in $@; do
    fileid_process ${i}
done
