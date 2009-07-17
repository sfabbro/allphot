# -*-bash-*-
#!/usr/bin/env bash


# very simple so far, needs more work
check_exist() {
    if [[ ! -f ${1} ]]; then
	echo " Error: file ${1} not found." >&2
	return -1
    fi
}

# return false if a catalog has less than 5 lines
check_catalog() {
    if [[ $(wc -l ${1} | cut -d ' ' -f1) -lt 5  ]]; then
	echo "*** Error: ${1} is not a valid catalog" >&2
	return -1
    fi
    return 0
}

# return false if fwhm from psf is not between 0.5 and 20
check_psf() {
    local ifwhm10=$(awk 'NR==2 {print int(($1+$2)*11.76); exit}' ${1} 2> /dev/null)    
    if [[ -z ${ifwhm10} ]] || [[ ${ifwhm10} -lt 5 ]] || [[ ${ifwhm10} -gt 200 ]]; then
	echo "*** Error: ${1} is not a valid PSF file, ${ifwhm10}" >&2
	return -1
    fi
    return 0
}

# get_fwhm <psf file>
get_fwhm() {
    awk 'NR==2 {print ($1+$2)*1.176; exit}' ${1} 2> /dev/null
}

daophot_process_init() {
    check_exist ${1} || return -1
    local image=$(basename ${1%.*})
    ALLPHOT_PROCESS_DIR=${PWD}/${image}_process    
    mkdir -p ${ALLPHOT_PROCESS_DIR}
    pushd ${ALLPHOT_PROCESS_DIR} &> /dev/null
    [[ ! -r daophot.opt ]] && cp ${ALLPHOT_OPT_DAOPHOT} daophot.opt
    [[ ! -r photo.opt ]] && cp ${ALLPHOT_OPT_PHOTO} photo.opt
    [[ ! -r allstar.opt ]] && cp ${ALLPHOT_OPT_ALLSTAR} allstar.opt
    [[ ! -r ${image}.fits ]] && ln -sfn ${1} ${image}.fits
    return 0
}


daophot_process_end() {
    popd &> /dev/null
#    for f in ${ALLPHOT_PROCESS_DIR}/* ; do
#	[[ ! -h ${f} ]] && mv -f ${f} ${ALLPHOT_PROCESS_DIR}/..
#    done
#    rm -rf ${ALLPHOT_PROCESS_DIR}
}

daophot_split_process() {
    local ncores=$(grep -c processor /proc/cpuinfo)
    echo " Number of core to split: ${ncores}"
    echo " Not implemented"
}
