# -*-bash-*-
#!/usr/bin/env bash

source ${ALLPHOT_EXEC_DIR}/process.bash
source ${ALLPHOT_EXEC_DIR}/daophot.bash
source ${ALLPHOT_EXEC_DIR}/allframe.bash
source ${ALLPHOT_EXEC_DIR}/options.bash

get_nstars() {
    wc -l ${1} | awk '{print $1 - 3}'
}

# remove_neighbors <neighbor file> <file to remove stars>
remove_neighbors() {
    local nstars=$(get_nstars ${2}) nnei=$(grep -c [\*\?] ${1})
    [[ ${nnei} -gt 0 ]] && \
	sed -i \
	    $(grep [\*\?] ${1} | awk '{printf "-e /^[[:space:]]*"$1"/d "}') \
	    ${2}
    echo " >>> Removed ${nnei} PSF stars neighbours"
}

# apply_chisharp_cuts <pick catalog> <profile fit catalog> [chimax] [sharpmax]
# from a profile fit 
# select stars with chi<chimax and |sharp|<sharpmax
apply_chisharp_cuts() {
    local pickcat=${1} profcat=${2} id cmax=${3:-2} smax=${4:-0.6}
    head -n 3 ${profcat} > ${pickcat}.filtered    
    while read line; do
	id=$(echo ${line} | awk '{print $1}')
	awk -v vid="${id}" \
	    -v vcmax="${cmax}" \
	    -v vsmax="${smax}" \
	    '{ if ( $1 == vid  && $8< vcmax && $9<vsmax && $9>-vsmax ) { print } }' \
	    ${profcat} >> ${pickcat}.filtered
    done < ${pickcat}
    local nleft=$(( $(get_nstars ${pickcat}) - $(get_nstars ${pickcat}.filtered) ))
    echo " >>> Removed ${nleft} objects with chi > ${cmax} and |sharp| > ${smax}"
    mv ${pickcat}.filtered ${pickcat}
}

allphot_filter_psf_stars() {
    echo " >>> ALLPHOT FILTER PSF STARS"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    if [[ -e ${image}.lst ]]; then
	[[ -e ${image}.nei ]] && remove_neighbors ${image}.nei ${image}.lst
	[[ -e ${image}.als ]] && apply_chisharp_cuts ${image}.lst ${image}.als ${3} ${4}
    else
	echo " Missing lst file for ${1}, no PSF star filtering"
    fi
    daophot_process_end
}

allphot_find_newstars() {
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    local images=${image}s
    echo " >>> DAOPHOT FIND"
    [[ -e ${image}.coo ]] && mv -f ${image}.coo ${image}.coo.old
    {
	daophot_attach ${images}
	daophot_find ${images}.coo
	daophot_attach ${image}
	daophot_phot_with_psf photo.opt ${catalog} ${image}.ap
	daophot_offset ${images}.ap \
	    $(tail -n 1 ${image}.coo | awk '{print $1+1}') 0 0 0 \
	    ${images}.off
	daophot_append ${image}.ap ${images}.off ${image}.cmb
    } > ${image}.subfind.in
    daophot < ${image}.subfind.in
    check_catalog ${image}.ap || mv -f ${image}.ap.old ${image}.ap
    rm -f ${image}jnk.fits ${image}.coo.old
    daophot_process_end
}


# allphot_opt <fits file> [<dictionary file>]
allphot_opt() {
    echo " >>> DAOPHOT OPT"
    local image=$(basename ${1%.*})
    local fitsfile=$(readlink -f ${1})
    local dictfile=$(readlink -f ${2:-${image}.dict})
    daophot_process_init ${1} || return -1
    [[ -r ${dictfile} ]] && option_update_from_dict ${dictfile} ${fitsfile}
    if [[ -e ${image}.psf ]] && check_psf ${image}.psf; then
	option_update_from_fwhm $(get_psf_fwhm ${image}.psf)
    fi
    echo > ${image}.opt.in
    daophot < ${image}.opt.in
    daophot_process_end
}

allphot_upgrade_psf() {
    echo " >>> DAOPHOT PSF UPGRADE"
    local image=$(basename ${1%.*})
    local fitsfile=$(readlink -f ${1})
    daophot_process_init ${1} || return -1
    if [[ -e ${image}.psf ]] && check_psf ${image}.psf; then
	option_upgrade_psf_model
    fi
    daophot_process_end
}

# allphot_find <fits> produce an image.coo file
allphot_find() {
    echo " >>> DAOPHOT FIND"
    daophot_process_init ${1} || return -1
    local image=$(basename ${1%.*})
    [[ -e ${image}.coo ]] && mv -f ${image}.coo ${image}.coo.old
    {
	daophot_attach ${image}
	daophot_find ${image}.coo
    } > ${image}.find.in
    daophot < ${image}.find.in
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
    daophot < ${image}.phot.in
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
    local magfaint=13
    [[ -e ${image}.lst ]] && mv -f ${image}.lst ${image}.lst.old    
    {
	daophot_pick ${catalog} ${nstars} ${magfaint} ${image}.lst
    } > ${image}.pick.in    
    daophot < ${image}.pick.in
    check_catalog ${image}.lst || mv -f ${image}.lst.old ${image}.lst
    daophot_process_end
}

# allphot_psf <fits> <input coordinate catalog> produces image.psf
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
    daophot < ${image}.psf.in
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
    [[ -e ${image}s.fits ]] && mv -f ${image}s.fits ${image}s.fits.old

    allstar < ${image}.allstar.in
    check_catalog ${image}.als || mv -f ${image}.als.old ${image}.als
    rm -f ${image}.als.old
    daophot_process_end
}

allphot_daomatch() {
    local field=${2%.*}
    check_exist ${1}
    echo " >>> DAOMATCH"
    daophot_match ${1} ${field}.mch ${3} > ${field}.daomatch.in
    daomatch < ${field}.daomatch.in
}

allphot_daomaster() {
    local field=${1%.*}
    echo " >>> DAOMASTER"
    [[ ! -r daomaster.opt ]] && cp -f ${ALLPHOT_OPT_DAOMASTER} daomaster.opt
    daophot_master ${field}.mch daomaster.opt > ${field}.daomaster.in
    daomaster < ${field}.daomaster.in
}

allphot_allframe() {
    local field=${1%.*}
    echo " >>> ALLFRAME"
    [[ ! -r allframe.opt ]] && cp -f ${ALLPHOT_OPT_ALLFRAME} allframe.opt
    daophot_allframe ${field}.mch ${field}.mag > ${field}.allframe.in
    allframe < ${field}.allframe.in
}

allphot_allstar_iterate() {
    allphot_opt ${1} $DICT
    allphot_find ${1}
    allphot_phot ${1}
    allphot_pick ${1}
    allphot_psf ${1}
    allphot_filter_psf_stars ${1}
    allphot_psf ${1}
    allphot_opt ${1} $DICT
    allphot_psf ${1}
    allphot_filter_psf_stars ${1}
    allphot_psf ${1}
    allphot_allstar ${1}
    allphot_pick ${1} als
    allphot_filter_psf_stars ${1}
    allphot_psf ${1} als
    allphot_filter_psf_stars ${1}
    allphot_upgrade_psf ${1}
    allphot_psf ${1} als
    allphot_filter_psf_stars ${1}
    allphot_psf ${1} als
    allphot_filter_psf_stars ${1}
    allphot_psf ${1} als
    allphot_allstar ${1}
}
