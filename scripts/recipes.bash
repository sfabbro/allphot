# -*-bash-*-
#!/usr/bin/env bash

source ${ALLPHOT_EXEC_DIR}/process.bash
source ${ALLPHOT_EXEC_DIR}/daophot.bash
source ${ALLPHOT_EXEC_DIR}/allframe.bash
source ${ALLPHOT_EXEC_DIR}/options.bash

# allphot_opt <fits file> [<assoc file>]
allphot_opt() {
    echo " >>> DAOPHOT OPT"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    local assfile=${2:-${image}.assoc}
    [[ -r ${assfile} ]] && option_update_from_fits ${1} ${assfile}
    if [[ -e ${image}.psf ]] && check_psf ${image}.psf; then
	option_update_from_fwhm $(get_fwhm ${image}.psf)
	option_update_psf_model
    fi
    echo > ${image}.opt.in
    ${ALLPHOT_BIN_DIR}/daophot < ${image}.opt.in
    daophot_process_end
}

# allphot_find <fits> produce an image.coo file
allphot_find() {@@@
    echo " >>> DAOPHOT FIND"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    [[ -e ${image}.coo ]] && mv -f ${image}.coo ${image}.coo.old
    {
	daophot_attach ${image}
	daophot_find ${image}.coo
    } > ${image}.find.in
    ${ALLPHOT_BIN_DIR}/daophot < ${image}.find.in
    check_catalog ${image}.coo || mv -f ${image}.coo.old ${image}.coo
    rm -f ${image}jnk.fits ${image}.coo.old
    daophot_process_end
}

# allphot_phot <fits> <input coordinate catalog> produce image.ap file
allphot_phot() {
    echo " >>> DAOPHOT PHOT"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    local catalog=${2:-${image}.coo}
    [[ -e ${image}.ap ]] &&  mv -f ${image}.ap ${image}.ap.old  
    {
	daophot_attach ${image}
	daophot_phot photo.opt ${catalog} ${image}.ap
    } > ${image}.phot.in
    ${ALLPHOT_BIN_DIR}/daophot < ${image}.phot.in
    check_catalog ${image}.ap || mv -f ${image}.ap.old ${image}.ap
    rm -f ${image}.ap.old
    daophot_process_end
}

allphot_pick() {
    echo " >>> DAOPHOT PICK"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    local catalog=${image}.${2:-ap}
    local nstars=200
    local magfaint=16
    [[ -e ${image}.lst ]] && mv -f ${image}.lst ${image}.lst.old    
    {
	daophot_pick ${catalog} ${nstars} ${magfaint} ${image}.lst
    } > ${image}.pick.in    
    ${ALLPHOT_BIN_DIR}/daophot < ${image}.pick.in
    check_catalog ${image}.lst || mv -f ${image}.lst.old ${image}.lst
    ## select stars with chi<2 and |sharp|<0.6
    if [[ ${2} == als ]]; then
	local id
	head -n 3 ${catalog} > ${catalog}.filtered
	while read line; do
	    id=$(echo ${line} | awk '{print $1}')
	    awk -v vid="${id}" '{ if ( $1 == vid  && $8<2 && $9<0.6 && $9>-0.6 ) { print } }' ${catalog} >> ${catalog}.filtered
	done < ${image}.lst
	echo "Selected $(wc -l ${catalog}.filtered | awk '{print $1}') stars out of $(wc -l ${image}.lst | awk '{print $1}')"
	mv ${catalog}.filtered ${image}.lst
    fi
    daophot_process_end
}

# allphot_psf <fits> <input coordinate catalog> produce image.psf
allphot_psf() {
    local image=$(basename ${1%.*})
    local catalog=${image}.${2:-ap}
    echo " >>> DAOPHOT PSF"
    daophot_process_init ${1} || return -1
    rm -f ${image}.psf
    [[ -e ${image}.psf ]] && mv -f ${image}.psf ${image}.psf.old 
    {
	daophot_attach ${image}
	daophot_psf ${catalog} ${image}.lst ${image}.psf
    } > ${image}.psf.in
    ${ALLPHOT_BIN_DIR}/daophot < ${image}.psf.in
    check_psf ${image}.psf || mv -f ${image}.psf.old ${image}.psf
    rm -f ${image}.psf.old
    daophot_process_end
}

# allphot_allstar <fits> [<input catalog> <psf file>]
allphot_allstar() {
    echo " >>> ALLSTAR"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    local incat=${2:-${image}.ap}
    local inpsf=${3:-${image}.psf}
    if ! check_catalog ${incat} || ! check_psf ${inpsf}; then
	daophot_process_end
	return -1
    fi
    daophot_allstar ${image} ${inpsf} ${incat} ${image}.als > ${image}.allstar.in
    [[ -e ${image}.als ]] && mv -f ${image}.als ${image}.als.old 
    ${ALLPHOT_BIN_DIR}/allstar < ${image}.allstar.in
    check_catalog ${image}.als || mv -f ${image}.als.old ${image}.als
    rm -f ${image}.als.old
    daophot_process_end
}

allphot_allstar_iterate() {
    allphot_opt ${1} ${2}
    allphot_find ${1}
    allphot_phot ${1}
    allphot_pick ${1}
    allphot_psf ${1}
    allphot_allstar ${1}
    allphot_opt ${1}
    allphot_pick ${1} als
    allphot_psf ${1} als
    allphot_allstar ${1}
}

allphot_daomatch() {
    local field=${2%.*}
    check_exist ${1}
    echo " >>> DAOMATCH"
    daophot_match ${1} ${field}.mch ${3} > ${field}.daomatch.in
    ${ALLPHOT_BIN_DIR}/daomatch < ${field}.daomatch.in
}

allphot_daomaster() {
    local field=${1%.*}
    echo " >>> DAOMASTER"
    [[ ! -r daomaster.opt ]] && cp -f ${ALLPHOT_OPT_DAOMASTER} daomaster.opt
    daophot_master ${field}.mch daomaster.opt > ${field}.daomaster.in
    ${ALLPHOT_BIN_DIR}/daomaster < ${field}.daomaster.in
}

allphot_allframe() {
    local field=${1%.*}
    echo " >>> ALLFRAME"
    [[ ! -r allframe.opt ]] && cp -f ${ALLPHOT_OPT_ALLFRAME} allframe.opt
    daophot_allframe ${field}.mch ${field}.mag > ${field}.allframe.in
    ${ALLPHOT_BIN_DIR}/allframe < ${field}.allframe.in
}
