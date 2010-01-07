# return false if a catalog has less than 5 lines
check_catalog() {
    [[ $(wc -l ${1} | cut -d ' ' -f1) -gt 5  ]] && return
    [[ -e ${1}.old ]]  && mv -f ${1}.old ${1}
    die -q "*** Error: ${1} is not a valid catalog"
}

# return false if fwhm of the psf file is not between 0.5 and 20
check_psf() {
    local ifwhm10=$(awk 'NR==2 {print int(($1+$2)*11.76); exit}' ${1} 2> /dev/null)    
    if [[ -z ${ifwhm10} ]] || [[ ${ifwhm10} -lt 5 ]] || [[ ${ifwhm10} -gt 200 ]]; then
	die -q "*** Error: ${1} is not a valid PSF file, ${ifwhm10}" >&2
    fi
}
